use charon_lib::ast::*;
use indoc::indoc;
use itertools::Itertools;

use crate::{
    GenerateCtx, build_branch, make_ocaml_ident, type_name_to_ocaml_ident, type_to_ocaml_call,
    type_to_ocaml_name,
};

pub const MANUAL_IMPLS: &[(&str, &str)] = &[
    (
        "charon_lib::ids::index_vec::IndexVec",
        "list_of_json arg1_of_json ctx json",
    ),
    // Hand-written because we filter out `None` values.
    (
        "charon_lib::ids::index_map::IndexMap",
        indoc!(
            r#"
            let* list = list_of_json (option_of_json arg1_of_json) ctx json in
            Ok (List.filter_map (fun x -> x) list)
            "#
        ),
    ),
    // Hand-written because we replace the `FileId` with the corresponding file name.
    (
        "FileId",
        indoc!(
            r#"
            let* file_id = FileId.id_of_json ctx json in
            let file = FileId.Map.find file_id ctx.id_to_file_map in
            Ok file
            "#,
        ),
    ),
    (
        "HashConsed",
        r#"Error "use `hash_consed_val_of_json` instead""#,
    ), // Not actually used
    (
        "Ty",
        "hash_consed_val_of_json ctx.ty_hashcons_map ty_kind_of_json ctx json",
    ),
    (
        "TraitRef",
        "hash_consed_val_of_json ctx.tref_hashcons_map trait_ref_contents_of_json ctx json",
    ),
];

fn build_function(ctx: &GenerateCtx, decl: &TypeDecl, branches: &str) -> String {
    let ty = TyKind::Adt(TypeDeclRef {
        id: TypeId::Adt(decl.def_id),
        generics: decl.generics.identity_args().into(),
    })
    .into_ty();
    let ty_name = type_name_to_ocaml_ident(&decl.item_meta);
    let ty = type_to_ocaml_name(ctx, &ty);
    let signature = if decl.generics.types.is_empty() {
        format!("{ty_name}_of_json (ctx : of_json_ctx) (js : json) : ({ty}, string) result =")
    } else {
        let types = &decl.generics.types;
        let gen_vars_space = types
            .iter()
            .enumerate()
            .map(|(i, _)| format!("'a{i}"))
            .join(" ");

        let mut args = Vec::new();
        let mut ty_args = Vec::new();
        for (i, _) in types.iter().enumerate() {
            args.push(format!("arg{i}_of_json"));
            ty_args.push(format!("(of_json_ctx -> json -> ('a{i}, string) result)"));
        }
        args.push("ctx".to_string());
        ty_args.push("of_json_ctx".to_string());
        args.push("js".to_string());
        ty_args.push("json".to_string());

        let ty_args = ty_args.into_iter().join(" -> ");
        let args = args.into_iter().join(" ");
        let fun_ty = format!("{gen_vars_space}. {ty_args} -> ({ty}, string) result");
        format!("{ty_name}_of_json : {fun_ty} = fun {args} ->")
    };
    format!(
        r#"
        and {signature}
          combine_error_msgs js __FUNCTION__
            (match js with{branches} | _ -> Error "")
        "#
    )
}

pub fn type_decl_to_deserializer(ctx: &GenerateCtx, decl: &TypeDecl) -> String {
    let return_ty = type_name_to_ocaml_ident(&decl.item_meta);
    let return_ty = if decl.generics.types.is_empty() {
        return_ty
    } else {
        format!("_ {return_ty}")
    };

    let branches = match &decl.kind {
        _ if let Some(def) = ctx.manual_json_impls.get(&decl.def_id) => format!("| json -> {def}"),
        TypeDeclKind::Struct(fields) if fields.is_empty() => {
            build_branch(ctx, "`Null", fields, "()")
        }
        TypeDeclKind::Struct(fields)
            if fields.len() == 1 && fields[0].name.as_ref().is_some_and(|name| name == "_raw") =>
        {
            // These are the special strongly-typed integers.
            let short_name = decl
                .item_meta
                .name
                .name
                .last()
                .unwrap()
                .as_ident()
                .unwrap()
                .0
                .clone();
            format!("| x -> {short_name}.id_of_json ctx x")
        }
        TypeDeclKind::Struct(fields)
            if fields.len() == 1
                && (fields[0].name.is_none()
                    || decl
                        .item_meta
                        .attr_info
                        .attributes
                        .iter()
                        .filter_map(|a| a.as_unknown())
                        .any(|a| a.to_string() == "serde(transparent)")) =>
        {
            let ty = &fields[0].ty;
            let call = type_to_ocaml_call(ctx, ty);
            format!("| x -> {call} ctx x")
        }
        TypeDeclKind::Alias(ty) => {
            let call = type_to_ocaml_call(ctx, ty);
            format!("| x -> {call} ctx x")
        }
        TypeDeclKind::Struct(fields) if fields.iter().all(|f| f.name.is_none()) => {
            let mut fields = fields.clone();
            for (i, f) in fields.iter_mut().enumerate() {
                f.name = Some(format!("x{i}"));
            }
            let pat: String = fields
                .iter()
                .map(|f| f.name.as_deref().unwrap())
                .map(|n| make_ocaml_ident(n))
                .join(";");
            let pat = format!("`List [ {pat} ]");
            let construct = fields
                .iter()
                .map(|f| f.renamed_name().unwrap())
                .map(|n| make_ocaml_ident(n))
                .join(", ");
            let construct = format!("( {construct} )");
            build_branch(ctx, &pat, &fields, &construct)
        }
        TypeDeclKind::Struct(fields) => {
            let fields = fields
                .iter()
                .filter(|field| {
                    !field
                        .attr_info
                        .attributes
                        .iter()
                        .filter_map(|a| a.as_unknown())
                        .any(|a| a.to_string() == "serde(skip)")
                })
                .collect_vec();
            let pat: String = fields
                .iter()
                .map(|f| {
                    let name = f.name.as_ref().unwrap();
                    let var = if f.is_opaque() {
                        "_"
                    } else {
                        &make_ocaml_ident(name)
                    };
                    format!("(\"{name}\", {var});")
                })
                .join("\n");
            let pat = format!("`Assoc [ {pat} ]");
            let construct = fields
                .iter()
                .filter(|f| !f.is_opaque())
                .map(|f| f.renamed_name().unwrap())
                .map(|n| make_ocaml_ident(n))
                .join("; ");
            let construct = format!("({{ {construct} }} : {return_ty})");
            build_branch(ctx, &pat, fields, &construct)
        }
        TypeDeclKind::Enum(variants) => {
            variants
                .iter()
                .filter(|v| !v.is_opaque())
                .map(|variant| {
                    let name = &variant.name;
                    let rename = variant.renamed_name();
                    if variant.fields.is_empty() {
                        // Unit variant
                        let pat = format!("`String \"{name}\"");
                        build_branch(ctx, &pat, &variant.fields, rename)
                    } else {
                        let mut fields = variant.fields.clone();
                        let inner_pat = if fields.iter().all(|f| f.name.is_none()) {
                            // Tuple variant
                            if variant.fields.len() == 1 {
                                let var = make_ocaml_ident(&variant.name);
                                fields[0].name = Some(var.clone());
                                var
                            } else {
                                for (i, f) in fields.iter_mut().enumerate() {
                                    f.name = Some(format!("x_{i}"));
                                }
                                let pat =
                                    fields.iter().map(|f| f.name.as_ref().unwrap()).join("; ");
                                format!("`List [ {pat} ]")
                            }
                        } else {
                            // Struct variant
                            let pat = fields
                                .iter()
                                .map(|f| {
                                    let name = f.name.as_ref().unwrap();
                                    let var = if f.is_opaque() {
                                        "_"
                                    } else {
                                        &make_ocaml_ident(name)
                                    };
                                    format!("(\"{name}\", {var});")
                                })
                                .join(" ");
                            format!("`Assoc [ {pat} ]")
                        };
                        let pat = format!("`Assoc [ (\"{name}\", {inner_pat}) ]");
                        let construct_fields = fields
                            .iter()
                            .map(|f| f.name.as_ref().unwrap())
                            .map(|n| make_ocaml_ident(n))
                            .join(", ");
                        let construct = format!("{rename} ({construct_fields})");
                        build_branch(ctx, &pat, &fields, &construct)
                    }
                })
                .join("\n")
        }
        TypeDeclKind::Union(..) => todo!(),
        TypeDeclKind::Opaque => todo!(),
        TypeDeclKind::Error(_) => todo!(),
    };
    build_function(ctx, decl, &branches)
}
