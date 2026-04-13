use charon_lib::ast::*;
use indoc::indoc;
use itertools::Itertools;

use crate::{GenerateCtx, make_ocaml_ident, type_name_to_ocaml_ident, type_to_ocaml_name};

pub const MANUAL_IMPLS: &[(&str, &str)] = &[
    (
        "charon_lib::ids::index_vec::IndexVec",
        "list_of_postcard arg1_of_postcard ctx state",
    ),
    // Hand-written because we filter out `None` values.
    (
        "charon_lib::ids::index_map::IndexMap",
        indoc!(
            r#"
            let* list = list_of_postcard (option_of_postcard arg1_of_postcard) ctx state in
            Ok (List.filter_map (fun x -> x) list)
            "#
        ),
    ),
    // Hand-written because we replace the `FileId` with the corresponding file name.
    (
        "FileId",
        indoc!(
            r#"
            let* file_id = FileId.id_of_postcard ctx state in
            let file = FileId.Map.find file_id ctx.id_to_file_map in
            Ok file
            "#,
        ),
    ),
    (
        "HashConsed",
        r#"Error "use `hash_consed_val_of_postcard` instead""#,
    ), // Not actually used
    (
        "Ty",
        "hash_consed_val_of_postcard ctx.ty_hashcons_map ty_kind_of_postcard ctx state",
    ),
    (
        "TraitRef",
        "hash_consed_val_of_postcard ctx.tref_hashcons_map trait_ref_contents_of_postcard ctx state",
    ),
];

fn build_function(ctx: &GenerateCtx, decl: &TypeDecl, body: &str) -> String {
    let ty = TyKind::Adt(TypeDeclRef {
        id: TypeId::Adt(decl.def_id),
        generics: decl.generics.identity_args().into(),
    })
    .into_ty();
    let ty_name = type_name_to_ocaml_ident(&decl.item_meta);
    let ty = type_to_ocaml_name(ctx, &ty);
    let signature = if decl.generics.types.is_empty() {
        format!(
            "{ty_name}_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) : ({ty}, string) result ="
        )
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
            args.push(format!("arg{i}_of_postcard"));
            ty_args.push(format!(
                "(of_postcard_ctx -> postcard_state -> ('a{i}, string) result)"
            ));
        }
        args.push("ctx".to_string());
        ty_args.push("of_postcard_ctx".to_string());
        args.push("state".to_string());
        ty_args.push("postcard_state".to_string());

        let ty_args = ty_args.into_iter().join(" -> ");
        let args = args.into_iter().join(" ");
        let fun_ty = format!("{gen_vars_space}. {ty_args} -> ({ty}, string) result");
        format!("{ty_name}_of_postcard : {fun_ty} = fun {args} ->")
    };
    format!(
        r#"
        and {signature}
          combine_postcard_error_msgs state __FUNCTION__
            ({body})
        "#
    )
}

/// Converts a type to the appropriate `*_of_postcard` call.
fn type_to_ocaml_postcard_call(ctx: &GenerateCtx, ty: &Ty) -> String {
    match ty.kind() {
        TyKind::Literal(LiteralTy::Bool) => "bool_of_postcard".to_string(),
        TyKind::Literal(LiteralTy::Char) => "char_of_postcard".to_string(),
        TyKind::Literal(LiteralTy::Int(int_ty)) => match int_ty {
            IntTy::I8 => "i8_of_postcard".to_string(),
            IntTy::I16 => "i16_of_postcard".to_string(),
            IntTy::I32 => "i32_of_postcard".to_string(),
            IntTy::I64 => "i64_of_postcard".to_string(),
            IntTy::I128 => "i128_of_postcard".to_string(),
            IntTy::Isize => "isize_of_postcard".to_string(),
        },
        TyKind::Literal(LiteralTy::UInt(uint_ty)) => match uint_ty {
            UIntTy::U8 => "u8_of_postcard".to_string(),
            UIntTy::U16 => "u16_of_postcard".to_string(),
            UIntTy::U32 => "u32_of_postcard".to_string(),
            UIntTy::U64 => "u64_of_postcard".to_string(),
            UIntTy::U128 => "u128_of_postcard".to_string(),
            UIntTy::Usize => "usize_of_postcard".to_string(),
        },
        TyKind::Literal(LiteralTy::Float(FloatTy::F16 | FloatTy::F32)) => {
            "f32_of_postcard".to_string()
        }
        TyKind::Literal(LiteralTy::Float(FloatTy::F64 | FloatTy::F128)) => {
            "f64_of_postcard".to_string()
        }
        TyKind::Adt(tref) => {
            let mut expr = Vec::new();
            for ty in &tref.generics.types {
                expr.push(type_to_ocaml_postcard_call(ctx, ty))
            }
            match tref.id {
                TypeId::Adt(id) => {
                    let mut first = if let Some(tdecl) = ctx.crate_data.type_decls.get(id) {
                        type_name_to_ocaml_ident(&tdecl.item_meta)
                    } else {
                        format!("missing_type_{id}")
                    };
                    if first == "vec" {
                        first = "list".to_string();
                    }
                    if first == "ustr" {
                        first = "string".to_string();
                    }
                    expr.insert(0, first + "_of_postcard");
                }
                TypeId::Builtin(BuiltinTy::Box) => expr.insert(0, "box_of_postcard".to_owned()),
                TypeId::Tuple => {
                    let name = match tref.generics.types.elem_count() {
                        2 => "pair_of_postcard".to_string(),
                        3 => "triple_of_postcard".to_string(),
                        len => format!("tuple_{len}_of_postcard"),
                    };
                    expr.insert(0, name);
                }
                _ => unimplemented!("{ty:?}"),
            }
            expr.into_iter().map(|f| format!("({f})")).join(" ")
        }
        TyKind::TypeVar(DeBruijnVar::Free(id)) => format!("arg{id}_of_postcard"),
        _ => unimplemented!("{ty:?}"),
    }
}

pub fn type_decl_to_deserializer(ctx: &GenerateCtx, decl: &TypeDecl) -> String {
    let return_ty = type_name_to_ocaml_ident(&decl.item_meta);
    let return_ty = if decl.generics.types.is_empty() {
        return_ty
    } else {
        format!("_ {return_ty}")
    };

    let body = match &decl.kind {
        _ if let Some(def) = ctx.manual_postcard_impls.get(&decl.def_id) => def.clone(),
        TypeDeclKind::Struct(fields) if fields.is_empty() => {
            format!("let* _ = unit_of_postcard ctx state in Ok (({return_ty}) : {return_ty})")
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
            format!("{short_name}.id_of_postcard ctx state")
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
            let call = type_to_ocaml_postcard_call(ctx, ty);
            format!("{call} ctx state")
        }
        TypeDeclKind::Alias(ty) => {
            let call = type_to_ocaml_postcard_call(ctx, ty);
            format!("{call} ctx state")
        }
        TypeDeclKind::Struct(fields) if fields.iter().all(|f| f.name.is_none()) => {
            let mut fields = fields.clone();
            for (i, f) in fields.iter_mut().enumerate() {
                f.name = Some(format!("x{i}"));
            }
            let convert = fields
                .iter()
                .filter(|f| !f.is_opaque())
                .map(|f| {
                    let rename = make_ocaml_ident(f.name.as_ref().unwrap());
                    let convert = type_to_ocaml_postcard_call(ctx, &f.ty);
                    format!("let* {rename} = {convert} ctx state in")
                })
                .join("\n");
            let construct = fields
                .iter()
                .filter(|f| !f.is_opaque())
                .map(|f| f.name.as_ref().unwrap())
                .map(|n| make_ocaml_ident(n))
                .join(", ");
            let construct = format!("( {construct} )");
            format!("{convert} Ok ({construct})")
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
            let convert = fields
                .iter()
                .filter(|f| !f.is_opaque())
                .map(|f| {
                    let rename = make_ocaml_ident(f.renamed_name().unwrap());
                    let convert = type_to_ocaml_postcard_call(ctx, &f.ty);
                    format!("let* {rename} = {convert} ctx state in")
                })
                .join("\n");
            let construct = fields
                .iter()
                .filter(|f| !f.is_opaque())
                .map(|f| f.renamed_name().unwrap())
                .map(|n| make_ocaml_ident(n))
                .join("; ");
            let construct = format!("({{ {construct} }} : {return_ty})");
            format!("{convert} Ok ({construct})")
        }
        TypeDeclKind::Enum(variants) => {
            let branches = variants
                .iter()
                .enumerate()
                .filter(|(_, v)| !v.is_opaque())
                .map(|(i, variant)| {
                    let rename = variant.renamed_name();
                    if variant.fields.is_empty() {
                        format!("| {i} -> Ok {rename}")
                    } else {
                        let mut fields = variant.fields.clone();
                        if fields.iter().all(|f| f.name.is_none()) {
                            if variant.fields.len() == 1 {
                                let var = make_ocaml_ident(&variant.name);
                                fields[0].name = Some(var);
                            } else {
                                for (j, f) in fields.iter_mut().enumerate() {
                                    f.name = Some(format!("x_{j}"));
                                }
                            }
                        }
                        let convert = fields
                            .iter()
                            .filter(|f| !f.is_opaque())
                            .map(|f| {
                                let rename = make_ocaml_ident(f.name.as_ref().unwrap());
                                let convert = type_to_ocaml_postcard_call(ctx, &f.ty);
                                format!("let* {rename} = {convert} ctx state in")
                            })
                            .join("\n");
                        let construct_fields = fields
                            .iter()
                            .filter(|f| !f.is_opaque())
                            .map(|f| f.name.as_ref().unwrap())
                            .map(|n| make_ocaml_ident(n))
                            .join(", ");
                        format!("| {i} -> {convert} Ok ({rename} ({construct_fields}))")
                    }
                })
                .join("\n");
            format!(
                    "let* tag = decode_varint_u32 state in
                     match tag with {branches}
                     | _ -> Error (\"Unmatched tag \" ^ (string_of_int tag) ^ \" for enum {return_ty}\")"
                )
        }
        TypeDeclKind::Union(..) => todo!(),
        TypeDeclKind::Opaque => todo!(),
        TypeDeclKind::Error(_) => todo!(),
    };
    build_function(ctx, decl, &body)
}
