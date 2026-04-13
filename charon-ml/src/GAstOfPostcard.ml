open OfPostcardBasic
open Identifiers
open Meta
open Values
open Types
open Scalars
open Expressions
open GAst
include Generated_GAstOfPostcard

let option_list_of_postcard of_postcard =
  list_of_postcard (option_of_postcard of_postcard)

let rec gfun_decl_of_postcard
    (body_of_postcard :
      of_postcard_ctx ->
      postcard_state ->
      ('body gexpr_body option, string) result) (ctx : of_postcard_ctx)
    (state : postcard_state) : ('body gfun_decl, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* def_id = FunDeclId.id_of_postcard ctx state in
     let* item_meta = item_meta_of_postcard ctx state in
     let* generics = generic_params_of_postcard ctx state in
     let* signature = fun_sig_of_postcard ctx state in
     let* src = item_source_of_postcard ctx state in
     let* is_global_initializer =
       option_of_postcard global_decl_id_of_postcard ctx state
     in
     let* body = body_of_postcard ctx state in
     Ok
       {
         def_id;
         item_meta;
         generics;
         signature;
         src;
         is_global_initializer;
         body;
       })

and id_to_file_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (of_postcard_ctx, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* files = list_of_postcard file_of_postcard ctx state in
     let files_with_ids =
       List.mapi (fun i file -> (FileId.of_int i, file)) files
     in
     let id_to_file_map = FileId.Map.of_list files_with_ids in
     Ok { ctx with id_to_file_map })

and gtranslated_crate_of_postcard
    (body_of_postcard :
      of_postcard_ctx ->
      postcard_state ->
      ('body gexpr_body option, string) result) (state : postcard_state) :
    ('body gcrate, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let ctx = empty_of_postcard_ctx in
     let* crate_name = string_of_postcard ctx state in
     let* options = cli_options_of_postcard ctx state in
     let* target_information = target_info_of_postcard ctx state in
     let* _item_names =
       list_of_postcard
         (key_value_pair_of_postcard item_id_of_postcard name_of_postcard)
         ctx state
     in
     let* _short_names =
       list_of_postcard
         (key_value_pair_of_postcard item_id_of_postcard name_of_postcard)
         ctx state
     in
     let* ctx = id_to_file_of_postcard ctx state in
     let* type_decls =
       option_list_of_postcard type_decl_of_postcard ctx state
     in
     let* fun_decls =
       option_list_of_postcard
         (gfun_decl_of_postcard body_of_postcard)
         ctx state
     in
     let* global_decls =
       option_list_of_postcard global_decl_of_postcard ctx state
     in
     let* trait_decls =
       option_list_of_postcard trait_decl_of_postcard ctx state
     in
     let* trait_impls =
       option_list_of_postcard trait_impl_of_postcard ctx state
     in
     let* unit_metadata = global_decl_ref_of_postcard ctx state in
     let* ordered_decls =
       list_of_postcard declaration_group_of_postcard ctx state
     in
     let type_decls = TypeDeclId.map_of_indexed_list type_decls in
     let fun_decls = FunDeclId.map_of_indexed_list fun_decls in
     let global_decls = GlobalDeclId.map_of_indexed_list global_decls in
     let trait_decls = TraitDeclId.map_of_indexed_list trait_decls in
     let trait_impls = TraitImplId.map_of_indexed_list trait_impls in
     Ok
       {
         name = crate_name;
         options;
         target_information;
         declarations = ordered_decls;
         type_decls;
         fun_decls;
         global_decls;
         trait_decls;
         trait_impls;
         unit_metadata;
       })

and gcrate_of_postcard
    (body_of_postcard :
      of_postcard_ctx ->
      postcard_state ->
      ('body gexpr_body option, string) result) (state : postcard_state) :
    ('body gcrate, string) result =
  let* charon_version = string_of_postcard () state in
  if not (String.equal charon_version CharonVersion.supported_charon_version)
  then
    Error
      ("Incompatible version of charon: this program supports llbc emitted by \
        charon v" ^ CharonVersion.supported_charon_version
     ^ " but attempted to read a file emitted by charon v" ^ charon_version
     ^ ".")
  else
    let* krate = gtranslated_crate_of_postcard body_of_postcard state in
    let* _has_errors = bool_of_postcard () state in
    Ok krate
