open OfPostcardBasic
open Identifiers
open Meta
open Values
open Types
open Scalars
open Expressions
open GAst
module FileId = IdGen ()
module HashConsId = IdGen ()

let log = Logging.llbc_of_json_logger

type id_to_file_map = file FileId.Map.t

type of_postcard_ctx = {
  id_to_file_map : id_to_file_map;
  ty_hashcons_map : ty HashConsId.Map.t ref;
  tref_hashcons_map : trait_ref HashConsId.Map.t ref;
}

let empty_of_postcard_ctx : of_postcard_ctx =
  {
    id_to_file_map = FileId.Map.empty;
    ty_hashcons_map = ref HashConsId.Map.empty;
    tref_hashcons_map = ref HashConsId.Map.empty;
  }

let hash_consed_val_of_postcard (map : 'a HashConsId.Map.t ref)
    (of_postcard : of_postcard_ctx -> postcard_state -> ('a, string) result)
    (ctx : of_postcard_ctx) (state : postcard_state) : ('a, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* id = decode_varint_usize state in
         let* v = of_postcard ctx state in
         let id = HashConsId.of_int id in
         map := HashConsId.Map.add id v !map;
         Ok v
     | 1 ->
         let* id = decode_varint_usize state in
         let id = HashConsId.of_int id in
         begin
           match HashConsId.Map.find_opt id !map with
           | Some v -> Ok v
           | None ->
               Error
                 "Hash-consing key not found; there is a serialization \
                  mismatch between Rust and OCaml"
         end
     | 2 -> of_postcard ctx state
     | _ -> Error "")

let path_buf_of_postcard = string_of_postcard

let big_int_of_postcard _ (state : postcard_state) : (big_int, string) result =
  i128_of_postcard () state

let rec ___ = ()

and abort_kind_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (abort_kind, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* panic = option_of_postcard name_of_postcard ctx state in
         Ok (Panic panic)
     | 1 -> Ok UndefinedBehavior
     | 2 -> Ok UnwindTerminate
     | _ -> Error ("Unmatched tag " ^ string_of_int tag ^ " for enum abort_kind"))

and aggregate_kind_of_postcard (ctx : of_postcard_ctx) (state : postcard_state)
    : (aggregate_kind, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* x_0 = type_decl_ref_of_postcard ctx state in
         let* x_1 = option_of_postcard variant_id_of_postcard ctx state in
         let* x_2 = option_of_postcard field_id_of_postcard ctx state in
         Ok (AggregatedAdt (x_0, x_1, x_2))
     | 1 ->
         let* x_0 = ty_of_postcard ctx state in
         let* x_1 = box_of_postcard constant_expr_of_postcard ctx state in
         Ok (AggregatedArray (x_0, x_1))
     | 2 ->
         let* x_0 = ty_of_postcard ctx state in
         let* x_1 = ref_kind_of_postcard ctx state in
         Ok (AggregatedRawPtr (x_0, x_1))
     | _ ->
         Error
           ("Unmatched tag " ^ string_of_int tag ^ " for enum aggregate_kind"))

and alignment_modifier_of_postcard (ctx : of_postcard_ctx)
    (state : postcard_state) : (alignment_modifier, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* align = u64_of_postcard ctx state in
         Ok (Align align)
     | 1 ->
         let* pack = u64_of_postcard ctx state in
         Ok (Pack pack)
     | _ ->
         Error
           ("Unmatched tag " ^ string_of_int tag
          ^ " for enum alignment_modifier"))

and assertion_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (assertion, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* cond = operand_of_postcard ctx state in
     let* expected = bool_of_postcard ctx state in
     let* check_kind =
       option_of_postcard builtin_assert_kind_of_postcard ctx state
     in
     Ok ({ cond; expected; check_kind } : assertion))

and attr_info_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (attr_info, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* attributes = list_of_postcard attribute_of_postcard ctx state in
     let* inline = option_of_postcard inline_attr_of_postcard ctx state in
     let* rename = option_of_postcard string_of_postcard ctx state in
     let* public = bool_of_postcard ctx state in
     Ok ({ attributes; inline; rename; public } : attr_info))

and attribute_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (attribute, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 -> Ok AttrOpaque
     | 1 -> Ok AttrExclude
     | 2 ->
         let* rename = string_of_postcard ctx state in
         Ok (AttrRename rename)
     | 3 ->
         let* variants_prefix = string_of_postcard ctx state in
         Ok (AttrVariantsPrefix variants_prefix)
     | 4 ->
         let* variants_suffix = string_of_postcard ctx state in
         Ok (AttrVariantsSuffix variants_suffix)
     | 5 ->
         let* doc_comment = string_of_postcard ctx state in
         Ok (AttrDocComment doc_comment)
     | 6 ->
         let* unknown = raw_attribute_of_postcard ctx state in
         Ok (AttrUnknown unknown)
     | _ -> Error ("Unmatched tag " ^ string_of_int tag ^ " for enum attribute"))

and binop_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (binop, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 -> Ok BitXor
     | 1 -> Ok BitAnd
     | 2 -> Ok BitOr
     | 3 -> Ok Eq
     | 4 -> Ok Lt
     | 5 -> Ok Le
     | 6 -> Ok Ne
     | 7 -> Ok Ge
     | 8 -> Ok Gt
     | 9 ->
         let* add = overflow_mode_of_postcard ctx state in
         Ok (Add add)
     | 10 ->
         let* sub = overflow_mode_of_postcard ctx state in
         Ok (Sub sub)
     | 11 ->
         let* mul = overflow_mode_of_postcard ctx state in
         Ok (Mul mul)
     | 12 ->
         let* div = overflow_mode_of_postcard ctx state in
         Ok (Div div)
     | 13 ->
         let* rem = overflow_mode_of_postcard ctx state in
         Ok (Rem rem)
     | 14 -> Ok AddChecked
     | 15 -> Ok SubChecked
     | 16 -> Ok MulChecked
     | 17 ->
         let* shl = overflow_mode_of_postcard ctx state in
         Ok (Shl shl)
     | 18 ->
         let* shr = overflow_mode_of_postcard ctx state in
         Ok (Shr shr)
     | 19 -> Ok Offset
     | 20 -> Ok Cmp
     | _ -> Error ("Unmatched tag " ^ string_of_int tag ^ " for enum binop"))

and binder_of_postcard :
    'a0.
    (of_postcard_ctx -> postcard_state -> ('a0, string) result) ->
    of_postcard_ctx ->
    postcard_state ->
    ('a0 binder, string) result =
 fun arg0_of_postcard ctx state ->
  combine_postcard_error_msgs state __FUNCTION__
    (let* binder_params = generic_params_of_postcard ctx state in
     let* binder_value = arg0_of_postcard ctx state in
     Ok ({ binder_params; binder_value } : _ binder))

and binder_kind_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (binder_kind, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* x_0 = trait_decl_id_of_postcard ctx state in
         let* x_1 = trait_item_name_of_postcard ctx state in
         Ok (BKTraitType (x_0, x_1))
     | 1 ->
         let* x_0 = trait_decl_id_of_postcard ctx state in
         let* x_1 = trait_item_name_of_postcard ctx state in
         Ok (BKTraitMethod (x_0, x_1))
     | 2 -> Ok BKInherentImplBlock
     | 3 -> Ok BKDyn
     | 4 -> Ok BKOther
     | _ ->
         Error ("Unmatched tag " ^ string_of_int tag ^ " for enum binder_kind"))

and borrow_kind_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (borrow_kind, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 -> Ok BShared
     | 1 -> Ok BMut
     | 2 -> Ok BTwoPhaseMut
     | 3 -> Ok BShallow
     | 4 -> Ok BUniqueImmutable
     | _ ->
         Error ("Unmatched tag " ^ string_of_int tag ^ " for enum borrow_kind"))

and builtin_assert_kind_of_postcard (ctx : of_postcard_ctx)
    (state : postcard_state) : (builtin_assert_kind, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* len = operand_of_postcard ctx state in
         let* index = operand_of_postcard ctx state in
         Ok (BoundsCheck (len, index))
     | 1 ->
         let* x_0 = binop_of_postcard ctx state in
         let* x_1 = operand_of_postcard ctx state in
         let* x_2 = operand_of_postcard ctx state in
         Ok (Overflow (x_0, x_1, x_2))
     | 2 ->
         let* overflow_neg = operand_of_postcard ctx state in
         Ok (OverflowNeg overflow_neg)
     | 3 ->
         let* division_by_zero = operand_of_postcard ctx state in
         Ok (DivisionByZero division_by_zero)
     | 4 ->
         let* remainder_by_zero = operand_of_postcard ctx state in
         Ok (RemainderByZero remainder_by_zero)
     | 5 ->
         let* required = operand_of_postcard ctx state in
         let* found = operand_of_postcard ctx state in
         Ok (MisalignedPointerDereference (required, found))
     | 6 -> Ok NullPointerDereference
     | 7 ->
         let* invalid_enum_construction = operand_of_postcard ctx state in
         Ok (InvalidEnumConstruction invalid_enum_construction)
     | _ ->
         Error
           ("Unmatched tag " ^ string_of_int tag
          ^ " for enum builtin_assert_kind"))

and builtin_fun_id_of_postcard (ctx : of_postcard_ctx) (state : postcard_state)
    : (builtin_fun_id, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 -> Ok BoxNew
     | 1 -> Ok ArrayToSliceShared
     | 2 -> Ok ArrayToSliceMut
     | 3 -> Ok ArrayRepeat
     | 4 ->
         let* index = builtin_index_op_of_postcard ctx state in
         Ok (Index index)
     | 5 ->
         let* ptr_from_parts = ref_kind_of_postcard ctx state in
         Ok (PtrFromParts ptr_from_parts)
     | _ ->
         Error
           ("Unmatched tag " ^ string_of_int tag ^ " for enum builtin_fun_id"))

and builtin_impl_data_of_postcard (ctx : of_postcard_ctx)
    (state : postcard_state) : (builtin_impl_data, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 -> Ok BuiltinSized
     | 1 -> Ok BuiltinMetaSized
     | 2 -> Ok BuiltinTuple
     | 3 -> Ok BuiltinPointee
     | 4 -> Ok BuiltinDiscriminantKind
     | 5 -> Ok BuiltinAuto
     | 6 -> Ok BuiltinNoopDestruct
     | 7 -> Ok BuiltinUntrackedDestruct
     | 8 -> Ok BuiltinFn
     | 9 -> Ok BuiltinFnMut
     | 10 -> Ok BuiltinFnOnce
     | 11 -> Ok BuiltinCopy
     | 12 -> Ok BuiltinClone
     | _ ->
         Error
           ("Unmatched tag " ^ string_of_int tag ^ " for enum builtin_impl_data"))

and builtin_index_op_of_postcard (ctx : of_postcard_ctx)
    (state : postcard_state) : (builtin_index_op, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* is_array = bool_of_postcard ctx state in
     let* mutability = ref_kind_of_postcard ctx state in
     let* is_range = bool_of_postcard ctx state in
     Ok ({ is_array; mutability; is_range } : builtin_index_op))

and builtin_ty_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (builtin_ty, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 -> Ok TBox
     | 1 -> Ok TStr
     | _ -> Error ("Unmatched tag " ^ string_of_int tag ^ " for enum builtin_ty"))

and byte_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (byte, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 -> Ok Uninit
     | 1 ->
         let* value = u8_of_postcard ctx state in
         Ok (Value value)
     | 2 ->
         let* x_0 = provenance_of_postcard ctx state in
         let* x_1 = u8_of_postcard ctx state in
         Ok (Provenance (x_0, x_1))
     | _ -> Error ("Unmatched tag " ^ string_of_int tag ^ " for enum byte"))

and call_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (call, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* func = fn_operand_of_postcard ctx state in
     let* args = list_of_postcard operand_of_postcard ctx state in
     let* dest = place_of_postcard ctx state in
     Ok ({ func; args; dest } : call))

and cast_kind_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (cast_kind, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* x_0 = literal_type_of_postcard ctx state in
         let* x_1 = literal_type_of_postcard ctx state in
         Ok (CastScalar (x_0, x_1))
     | 1 ->
         let* x_0 = ty_of_postcard ctx state in
         let* x_1 = ty_of_postcard ctx state in
         Ok (CastRawPtr (x_0, x_1))
     | 2 ->
         let* x_0 = ty_of_postcard ctx state in
         let* x_1 = ty_of_postcard ctx state in
         Ok (CastFnPtr (x_0, x_1))
     | 3 ->
         let* x_0 = ty_of_postcard ctx state in
         let* x_1 = ty_of_postcard ctx state in
         let* x_2 = unsizing_metadata_of_postcard ctx state in
         Ok (CastUnsize (x_0, x_1, x_2))
     | 4 ->
         let* x_0 = ty_of_postcard ctx state in
         let* x_1 = ty_of_postcard ctx state in
         Ok (CastTransmute (x_0, x_1))
     | 5 ->
         let* x_0 = ty_of_postcard ctx state in
         let* x_1 = ty_of_postcard ctx state in
         Ok (CastConcretize (x_0, x_1))
     | _ -> Error ("Unmatched tag " ^ string_of_int tag ^ " for enum cast_kind"))

and cli_options_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (cli_options, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* ullbc = bool_of_postcard ctx state in
     let* precise_drops = bool_of_postcard ctx state in
     let* skip_borrowck = bool_of_postcard ctx state in
     let* mir = option_of_postcard mir_level_of_postcard ctx state in
     let* rustc_args = list_of_postcard string_of_postcard ctx state in
     let* monomorphize = bool_of_postcard ctx state in
     let* monomorphize_mut =
       option_of_postcard monomorphize_mut_of_postcard ctx state
     in
     let* start_from = list_of_postcard string_of_postcard ctx state in
     let* start_from_attribute =
       option_of_postcard string_of_postcard ctx state
     in
     let* start_from_pub = bool_of_postcard ctx state in
     let* included = list_of_postcard string_of_postcard ctx state in
     let* opaque = list_of_postcard string_of_postcard ctx state in
     let* exclude = list_of_postcard string_of_postcard ctx state in
     let* extract_opaque_bodies = bool_of_postcard ctx state in
     let* translate_all_methods = bool_of_postcard ctx state in
     let* lift_associated_types =
       list_of_postcard string_of_postcard ctx state
     in
     let* hide_marker_traits = bool_of_postcard ctx state in
     let* remove_adt_clauses = bool_of_postcard ctx state in
     let* hide_allocator = bool_of_postcard ctx state in
     let* remove_unused_self_clauses = bool_of_postcard ctx state in
     let* desugar_drops = bool_of_postcard ctx state in
     let* ops_to_function_calls = bool_of_postcard ctx state in
     let* index_to_function_calls = bool_of_postcard ctx state in
     let* treat_box_as_builtin = bool_of_postcard ctx state in
     let* raw_consts = bool_of_postcard ctx state in
     let* unsized_strings = bool_of_postcard ctx state in
     let* reconstruct_fallible_operations = bool_of_postcard ctx state in
     let* reconstruct_asserts = bool_of_postcard ctx state in
     let* unbind_item_vars = bool_of_postcard ctx state in
     let* print_original_ullbc = bool_of_postcard ctx state in
     let* print_ullbc = bool_of_postcard ctx state in
     let* print_built_llbc = bool_of_postcard ctx state in
     let* print_llbc = bool_of_postcard ctx state in
     let* dest_dir = option_of_postcard path_buf_of_postcard ctx state in
     let* dest_file = option_of_postcard path_buf_of_postcard ctx state in
     let* no_dedup_serialized_ast = bool_of_postcard ctx state in
     let* no_sources = bool_of_postcard ctx state in
     let* no_serialize = bool_of_postcard ctx state in
     let* format = option_of_postcard format_of_postcard ctx state in
     let* abort_on_error = bool_of_postcard ctx state in
     let* error_on_warnings = bool_of_postcard ctx state in
     let* preset = option_of_postcard preset_of_postcard ctx state in
     Ok
       ({
          ullbc;
          precise_drops;
          skip_borrowck;
          mir;
          rustc_args;
          monomorphize;
          monomorphize_mut;
          start_from;
          start_from_attribute;
          start_from_pub;
          included;
          opaque;
          exclude;
          extract_opaque_bodies;
          translate_all_methods;
          lift_associated_types;
          hide_marker_traits;
          remove_adt_clauses;
          hide_allocator;
          remove_unused_self_clauses;
          desugar_drops;
          ops_to_function_calls;
          index_to_function_calls;
          treat_box_as_builtin;
          raw_consts;
          unsized_strings;
          reconstruct_fallible_operations;
          reconstruct_asserts;
          unbind_item_vars;
          print_original_ullbc;
          print_ullbc;
          print_built_llbc;
          print_llbc;
          dest_dir;
          dest_file;
          no_dedup_serialized_ast;
          no_sources;
          no_serialize;
          format;
          abort_on_error;
          error_on_warnings;
          preset;
        }
         : cli_options))

and closure_info_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (closure_info, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* kind = closure_kind_of_postcard ctx state in
     let* fn_once_impl =
       region_binder_of_postcard trait_impl_ref_of_postcard ctx state
     in
     let* fn_mut_impl =
       option_of_postcard
         (region_binder_of_postcard trait_impl_ref_of_postcard)
         ctx state
     in
     let* fn_impl =
       option_of_postcard
         (region_binder_of_postcard trait_impl_ref_of_postcard)
         ctx state
     in
     let* signature = region_binder_of_postcard fun_sig_of_postcard ctx state in
     Ok ({ kind; fn_once_impl; fn_mut_impl; fn_impl; signature } : closure_info))

and closure_kind_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (closure_kind, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 -> Ok Fn
     | 1 -> Ok FnMut
     | 2 -> Ok FnOnce
     | _ ->
         Error ("Unmatched tag " ^ string_of_int tag ^ " for enum closure_kind"))

and const_generic_param_of_postcard (ctx : of_postcard_ctx)
    (state : postcard_state) : (const_generic_param, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* index = const_generic_var_id_of_postcard ctx state in
     let* name = string_of_postcard ctx state in
     let* ty = ty_of_postcard ctx state in
     Ok ({ index; name; ty } : const_generic_param))

and const_generic_var_id_of_postcard (ctx : of_postcard_ctx)
    (state : postcard_state) : (const_generic_var_id, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (ConstGenericVarId.id_of_postcard ctx state)

and constant_expr_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (constant_expr, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* kind = constant_expr_kind_of_postcard ctx state in
     let* ty = ty_of_postcard ctx state in
     Ok ({ kind; ty } : constant_expr))

and constant_expr_kind_of_postcard (ctx : of_postcard_ctx)
    (state : postcard_state) : (constant_expr_kind, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* literal = literal_of_postcard ctx state in
         Ok (CLiteral literal)
     | 1 ->
         let* x_0 = option_of_postcard variant_id_of_postcard ctx state in
         let* x_1 = list_of_postcard constant_expr_of_postcard ctx state in
         Ok (CAdt (x_0, x_1))
     | 2 ->
         let* array = list_of_postcard constant_expr_of_postcard ctx state in
         Ok (CArray array)
     | 3 ->
         let* global = global_decl_ref_of_postcard ctx state in
         Ok (CGlobal global)
     | 4 ->
         let* x_0 = trait_ref_of_postcard ctx state in
         let* x_1 = trait_item_name_of_postcard ctx state in
         Ok (CTraitConst (x_0, x_1))
     | 5 ->
         let* v_table_ref = trait_ref_of_postcard ctx state in
         Ok (CVTableRef v_table_ref)
     | 6 ->
         let* x_0 = box_of_postcard constant_expr_of_postcard ctx state in
         let* x_1 =
           option_of_postcard unsizing_metadata_of_postcard ctx state
         in
         Ok (CRef (x_0, x_1))
     | 7 ->
         let* x_0 = ref_kind_of_postcard ctx state in
         let* x_1 = box_of_postcard constant_expr_of_postcard ctx state in
         let* x_2 =
           option_of_postcard unsizing_metadata_of_postcard ctx state
         in
         Ok (CPtr (x_0, x_1, x_2))
     | 8 ->
         let* var =
           de_bruijn_var_of_postcard const_generic_var_id_of_postcard ctx state
         in
         Ok (CVar var)
     | 9 ->
         let* fn_def = fn_ptr_of_postcard ctx state in
         Ok (CFnDef fn_def)
     | 10 ->
         let* fn_ptr = fn_ptr_of_postcard ctx state in
         Ok (CFnPtr fn_ptr)
     | 11 ->
         let* ptr_no_provenance = u128_of_postcard ctx state in
         Ok (CPtrNoProvenance ptr_no_provenance)
     | 12 ->
         let* raw_memory = list_of_postcard byte_of_postcard ctx state in
         Ok (CRawMemory raw_memory)
     | 13 ->
         let* opaque = string_of_postcard ctx state in
         Ok (COpaque opaque)
     | _ ->
         Error
           ("Unmatched tag " ^ string_of_int tag
          ^ " for enum constant_expr_kind"))

and copy_non_overlapping_of_postcard (ctx : of_postcard_ctx)
    (state : postcard_state) : (copy_non_overlapping, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* src = operand_of_postcard ctx state in
     let* dst = operand_of_postcard ctx state in
     let* count = operand_of_postcard ctx state in
     Ok ({ src; dst; count } : copy_non_overlapping))

and de_bruijn_id_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (de_bruijn_id, string) result =
  combine_postcard_error_msgs state __FUNCTION__ (usize_of_postcard ctx state)

and de_bruijn_var_of_postcard :
    'a0.
    (of_postcard_ctx -> postcard_state -> ('a0, string) result) ->
    of_postcard_ctx ->
    postcard_state ->
    ('a0 de_bruijn_var, string) result =
 fun arg0_of_postcard ctx state ->
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* x_0 = de_bruijn_id_of_postcard ctx state in
         let* x_1 = arg0_of_postcard ctx state in
         Ok (Bound (x_0, x_1))
     | 1 ->
         let* free = arg0_of_postcard ctx state in
         Ok (Free free)
     | _ ->
         Error
           ("Unmatched tag " ^ string_of_int tag ^ " for enum _ de_bruijn_var"))

and declaration_group_of_postcard (ctx : of_postcard_ctx)
    (state : postcard_state) : (declaration_group, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* type_ =
           g_declaration_group_of_postcard type_decl_id_of_postcard ctx state
         in
         Ok (TypeGroup type_)
     | 1 ->
         let* fun_ =
           g_declaration_group_of_postcard fun_decl_id_of_postcard ctx state
         in
         Ok (FunGroup fun_)
     | 2 ->
         let* global =
           g_declaration_group_of_postcard global_decl_id_of_postcard ctx state
         in
         Ok (GlobalGroup global)
     | 3 ->
         let* trait_decl =
           g_declaration_group_of_postcard trait_decl_id_of_postcard ctx state
         in
         Ok (TraitDeclGroup trait_decl)
     | 4 ->
         let* trait_impl =
           g_declaration_group_of_postcard trait_impl_id_of_postcard ctx state
         in
         Ok (TraitImplGroup trait_impl)
     | 5 ->
         let* mixed =
           g_declaration_group_of_postcard item_id_of_postcard ctx state
         in
         Ok (MixedGroup mixed)
     | _ ->
         Error
           ("Unmatched tag " ^ string_of_int tag ^ " for enum declaration_group"))

and disambiguator_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (disambiguator, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (Disambiguator.id_of_postcard ctx state)

and discriminant_layout_of_postcard (ctx : of_postcard_ctx)
    (state : postcard_state) : (discriminant_layout, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* offset = u64_of_postcard ctx state in
     let* tag_ty = integer_type_of_postcard ctx state in
     let* encoding = tag_encoding_of_postcard ctx state in
     Ok ({ offset; tag_ty; encoding } : discriminant_layout))

and drop_kind_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (drop_kind, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 -> Ok Precise
     | 1 -> Ok Conditional
     | _ -> Error ("Unmatched tag " ^ string_of_int tag ^ " for enum drop_kind"))

and dyn_predicate_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (dyn_predicate, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* binder = binder_of_postcard ty_of_postcard ctx state in
     Ok ({ binder } : dyn_predicate))

and error_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (error, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* span = span_of_postcard ctx state in
     let* msg = string_of_postcard ctx state in
     Ok ({ span; msg } : error))

and field_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (field, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* span = span_of_postcard ctx state in
     let* attr_info = attr_info_of_postcard ctx state in
     let* field_name = option_of_postcard string_of_postcard ctx state in
     let* field_ty = ty_of_postcard ctx state in
     Ok ({ span; attr_info; field_name; field_ty } : field))

and field_id_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (field_id, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (FieldId.id_of_postcard ctx state)

and field_proj_kind_of_postcard (ctx : of_postcard_ctx) (state : postcard_state)
    : (field_proj_kind, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* x_0 = type_decl_id_of_postcard ctx state in
         let* x_1 = option_of_postcard variant_id_of_postcard ctx state in
         Ok (ProjAdt (x_0, x_1))
     | 1 ->
         let* tuple = usize_of_postcard ctx state in
         Ok (ProjTuple tuple)
     | _ ->
         Error
           ("Unmatched tag " ^ string_of_int tag ^ " for enum field_proj_kind"))

and file_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (file, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* name = file_name_of_postcard ctx state in
     let* crate_name = string_of_postcard ctx state in
     let* contents = option_of_postcard string_of_postcard ctx state in
     Ok ({ name; crate_name; contents } : file))

and file_id_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (file_id, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* file_id = FileId.id_of_postcard ctx state in
     let file = FileId.Map.find file_id ctx.id_to_file_map in
     Ok file)

and file_name_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (file_name, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* virtual_ = path_buf_of_postcard ctx state in
         Ok (Virtual virtual_)
     | 1 ->
         let* local = path_buf_of_postcard ctx state in
         Ok (Local local)
     | 2 ->
         let* not_real = string_of_postcard ctx state in
         Ok (NotReal not_real)
     | _ -> Error ("Unmatched tag " ^ string_of_int tag ^ " for enum file_name"))

and float_type_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (float_type, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 -> Ok F16
     | 1 -> Ok F32
     | 2 -> Ok F64
     | 3 -> Ok F128
     | _ -> Error ("Unmatched tag " ^ string_of_int tag ^ " for enum float_type"))

and float_value_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (float_value, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* float_value = string_of_postcard ctx state in
     let* float_ty = float_type_of_postcard ctx state in
     Ok ({ float_value; float_ty } : float_value))

and fn_operand_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (fn_operand, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* regular = fn_ptr_of_postcard ctx state in
         Ok (FnOpRegular regular)
     | 1 ->
         let* dynamic = operand_of_postcard ctx state in
         Ok (FnOpDynamic dynamic)
     | _ -> Error ("Unmatched tag " ^ string_of_int tag ^ " for enum fn_operand"))

and fn_ptr_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (fn_ptr, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* kind = box_of_postcard fn_ptr_kind_of_postcard ctx state in
     let* generics = box_of_postcard generic_args_of_postcard ctx state in
     Ok ({ kind; generics } : fn_ptr))

and fn_ptr_kind_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (fn_ptr_kind, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* fun_ = fun_id_of_postcard ctx state in
         Ok (FunId fun_)
     | 1 ->
         let* x_0 = trait_ref_of_postcard ctx state in
         let* x_1 = trait_item_name_of_postcard ctx state in
         let* x_2 = fun_decl_id_of_postcard ctx state in
         Ok (TraitMethod (x_0, x_1, x_2))
     | _ ->
         Error ("Unmatched tag " ^ string_of_int tag ^ " for enum fn_ptr_kind"))

and format_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (format, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 -> Ok Json
     | 1 -> Ok Postcard
     | _ -> Error ("Unmatched tag " ^ string_of_int tag ^ " for enum format"))

and fun_decl_id_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (fun_decl_id, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (FunDeclId.id_of_postcard ctx state)

and fun_decl_ref_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (fun_decl_ref, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* id = fun_decl_id_of_postcard ctx state in
     let* generics = box_of_postcard generic_args_of_postcard ctx state in
     Ok ({ id; generics } : fun_decl_ref))

and fun_id_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (fun_id, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* regular = fun_decl_id_of_postcard ctx state in
         Ok (FRegular regular)
     | 1 ->
         let* builtin = builtin_fun_id_of_postcard ctx state in
         Ok (FBuiltin builtin)
     | _ -> Error ("Unmatched tag " ^ string_of_int tag ^ " for enum fun_id"))

and fun_sig_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (fun_sig, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* is_unsafe = bool_of_postcard ctx state in
     let* inputs = list_of_postcard ty_of_postcard ctx state in
     let* output = ty_of_postcard ctx state in
     Ok ({ is_unsafe; inputs; output } : fun_sig))

and g_declaration_group_of_postcard :
    'a0.
    (of_postcard_ctx -> postcard_state -> ('a0, string) result) ->
    of_postcard_ctx ->
    postcard_state ->
    ('a0 g_declaration_group, string) result =
 fun arg0_of_postcard ctx state ->
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* non_rec = arg0_of_postcard ctx state in
         Ok (NonRecGroup non_rec)
     | 1 ->
         let* rec_ = list_of_postcard arg0_of_postcard ctx state in
         Ok (RecGroup rec_)
     | _ ->
         Error
           ("Unmatched tag " ^ string_of_int tag
          ^ " for enum _ g_declaration_group"))

and gexpr_body_of_postcard :
    'a0.
    (of_postcard_ctx -> postcard_state -> ('a0, string) result) ->
    of_postcard_ctx ->
    postcard_state ->
    ('a0 gexpr_body, string) result =
 fun arg0_of_postcard ctx state ->
  combine_postcard_error_msgs state __FUNCTION__
    (let* span = span_of_postcard ctx state in
     let* bound_body_regions = usize_of_postcard ctx state in
     let* locals = locals_of_postcard ctx state in
     let* body = arg0_of_postcard ctx state in
     Ok ({ span; bound_body_regions; locals; body } : _ gexpr_body))

and generic_args_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (generic_args, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* regions =
       index_map_of_postcard region_id_of_postcard region_of_postcard ctx state
     in
     let* types =
       index_map_of_postcard type_var_id_of_postcard ty_of_postcard ctx state
     in
     let* const_generics =
       index_map_of_postcard const_generic_var_id_of_postcard
         constant_expr_of_postcard ctx state
     in
     let* trait_refs =
       index_map_of_postcard trait_clause_id_of_postcard trait_ref_of_postcard
         ctx state
     in
     Ok ({ regions; types; const_generics; trait_refs } : generic_args))

and generic_params_of_postcard (ctx : of_postcard_ctx) (state : postcard_state)
    : (generic_params, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* regions =
       index_map_of_postcard region_id_of_postcard region_param_of_postcard ctx
         state
     in
     let* types =
       index_map_of_postcard type_var_id_of_postcard type_param_of_postcard ctx
         state
     in
     let* const_generics =
       index_map_of_postcard const_generic_var_id_of_postcard
         const_generic_param_of_postcard ctx state
     in
     let* trait_clauses =
       index_map_of_postcard trait_clause_id_of_postcard trait_param_of_postcard
         ctx state
     in
     let* regions_outlive =
       list_of_postcard
         (region_binder_of_postcard
            (outlives_pred_of_postcard region_of_postcard region_of_postcard))
         ctx state
     in
     let* types_outlive =
       list_of_postcard
         (region_binder_of_postcard
            (outlives_pred_of_postcard ty_of_postcard region_of_postcard))
         ctx state
     in
     let* trait_type_constraints =
       index_map_of_postcard trait_type_constraint_id_of_postcard
         (region_binder_of_postcard trait_type_constraint_of_postcard)
         ctx state
     in
     Ok
       ({
          regions;
          types;
          const_generics;
          trait_clauses;
          regions_outlive;
          types_outlive;
          trait_type_constraints;
        }
         : generic_params))

and global_decl_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (global_decl, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* def_id = global_decl_id_of_postcard ctx state in
     let* item_meta = item_meta_of_postcard ctx state in
     let* generics = generic_params_of_postcard ctx state in
     let* ty = ty_of_postcard ctx state in
     let* src = item_source_of_postcard ctx state in
     let* global_kind = global_kind_of_postcard ctx state in
     let* init = fun_decl_id_of_postcard ctx state in
     Ok
       ({ def_id; item_meta; generics; ty; src; global_kind; init }
         : global_decl))

and global_decl_id_of_postcard (ctx : of_postcard_ctx) (state : postcard_state)
    : (global_decl_id, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (GlobalDeclId.id_of_postcard ctx state)

and global_decl_ref_of_postcard (ctx : of_postcard_ctx) (state : postcard_state)
    : (global_decl_ref, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* id = global_decl_id_of_postcard ctx state in
     let* generics = box_of_postcard generic_args_of_postcard ctx state in
     Ok ({ id; generics } : global_decl_ref))

and global_kind_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (global_kind, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 -> Ok Static
     | 1 -> Ok NamedConst
     | 2 -> Ok AnonConst
     | _ ->
         Error ("Unmatched tag " ^ string_of_int tag ^ " for enum global_kind"))

and hash_consed_of_postcard :
    'a0.
    (of_postcard_ctx -> postcard_state -> ('a0, string) result) ->
    of_postcard_ctx ->
    postcard_state ->
    ('a0 hash_consed, string) result =
 fun arg0_of_postcard ctx state ->
  combine_postcard_error_msgs state __FUNCTION__
    (Error "use `hash_consed_val_of_postcard` instead")

and impl_elem_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (impl_elem, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* ty = binder_of_postcard ty_of_postcard ctx state in
         Ok (ImplElemTy ty)
     | 1 ->
         let* trait = trait_impl_id_of_postcard ctx state in
         Ok (ImplElemTrait trait)
     | _ -> Error ("Unmatched tag " ^ string_of_int tag ^ " for enum impl_elem"))

and index_map_of_postcard :
    'a0 'a1.
    (of_postcard_ctx -> postcard_state -> ('a0, string) result) ->
    (of_postcard_ctx -> postcard_state -> ('a1, string) result) ->
    of_postcard_ctx ->
    postcard_state ->
    ('a1 list, string) result =
 fun arg0_of_postcard arg1_of_postcard ctx state ->
  combine_postcard_error_msgs state __FUNCTION__
    (let* list =
       list_of_postcard (option_of_postcard arg1_of_postcard) ctx state
     in
     Ok (List.filter_map (fun x -> x) list))

and index_vec_of_postcard :
    'a0 'a1.
    (of_postcard_ctx -> postcard_state -> ('a0, string) result) ->
    (of_postcard_ctx -> postcard_state -> ('a1, string) result) ->
    of_postcard_ctx ->
    postcard_state ->
    ('a1 list, string) result =
 fun arg0_of_postcard arg1_of_postcard ctx state ->
  combine_postcard_error_msgs state __FUNCTION__
    (list_of_postcard arg1_of_postcard ctx state)

and inline_attr_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (inline_attr, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 -> Ok Hint
     | 1 -> Ok Never
     | 2 -> Ok Always
     | _ ->
         Error ("Unmatched tag " ^ string_of_int tag ^ " for enum inline_attr"))

and int_ty_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (int_ty, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 -> Ok Isize
     | 1 -> Ok I8
     | 2 -> Ok I16
     | 3 -> Ok I32
     | 4 -> Ok I64
     | 5 -> Ok I128
     | _ -> Error ("Unmatched tag " ^ string_of_int tag ^ " for enum int_ty"))

and integer_type_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (integer_type, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* signed = int_ty_of_postcard ctx state in
         Ok (Signed signed)
     | 1 ->
         let* unsigned = u_int_ty_of_postcard ctx state in
         Ok (Unsigned unsigned)
     | _ ->
         Error ("Unmatched tag " ^ string_of_int tag ^ " for enum integer_type"))

and item_id_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (item_id, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* type_ = type_decl_id_of_postcard ctx state in
         Ok (IdType type_)
     | 1 ->
         let* trait_decl = trait_decl_id_of_postcard ctx state in
         Ok (IdTraitDecl trait_decl)
     | 2 ->
         let* trait_impl = trait_impl_id_of_postcard ctx state in
         Ok (IdTraitImpl trait_impl)
     | 3 ->
         let* fun_ = fun_decl_id_of_postcard ctx state in
         Ok (IdFun fun_)
     | 4 ->
         let* global = global_decl_id_of_postcard ctx state in
         Ok (IdGlobal global)
     | _ -> Error ("Unmatched tag " ^ string_of_int tag ^ " for enum item_id"))

and item_meta_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (item_meta, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* name = name_of_postcard ctx state in
     let* span = span_of_postcard ctx state in
     let* source_text = option_of_postcard string_of_postcard ctx state in
     let* attr_info = attr_info_of_postcard ctx state in
     let* is_local = bool_of_postcard ctx state in
     let* lang_item = option_of_postcard string_of_postcard ctx state in
     Ok
       ({ name; span; source_text; attr_info; is_local; lang_item } : item_meta))

and item_source_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (item_source, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 -> Ok TopLevelItem
     | 1 ->
         let* info = closure_info_of_postcard ctx state in
         Ok (ClosureItem info)
     | 2 ->
         let* trait_ref = trait_decl_ref_of_postcard ctx state in
         let* item_name = trait_item_name_of_postcard ctx state in
         let* has_default = bool_of_postcard ctx state in
         Ok (TraitDeclItem (trait_ref, item_name, has_default))
     | 3 ->
         let* impl_ref = trait_impl_ref_of_postcard ctx state in
         let* trait_ref = trait_decl_ref_of_postcard ctx state in
         let* item_name = trait_item_name_of_postcard ctx state in
         let* reuses_default = bool_of_postcard ctx state in
         Ok (TraitImplItem (impl_ref, trait_ref, item_name, reuses_default))
     | 4 ->
         let* dyn_predicate = dyn_predicate_of_postcard ctx state in
         let* field_map =
           index_vec_of_postcard field_id_of_postcard v_table_field_of_postcard
             ctx state
         in
         let* supertrait_map =
           index_map_of_postcard trait_clause_id_of_postcard
             (option_of_postcard field_id_of_postcard)
             ctx state
         in
         Ok (VTableTyItem (dyn_predicate, field_map, supertrait_map))
     | 5 ->
         let* impl_ref = trait_impl_ref_of_postcard ctx state in
         Ok (VTableInstanceItem impl_ref)
     | 6 -> Ok VTableMethodShimItem
     | 7 -> Ok VTableInstanceMonoItem
     | 8 ->
         let* x_0 = trait_decl_id_of_postcard ctx state in
         let* x_1 = trait_item_name_of_postcard ctx state in
         let* x_2 = list_of_postcard ty_of_postcard ctx state in
         Ok (VTableMethodPreShimItem (x_0, x_1, x_2))
     | _ ->
         Error ("Unmatched tag " ^ string_of_int tag ^ " for enum item_source"))

and layout_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (layout, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* size = option_of_postcard u64_of_postcard ctx state in
     let* align = option_of_postcard u64_of_postcard ctx state in
     let* discriminant_layout =
       option_of_postcard discriminant_layout_of_postcard ctx state
     in
     let* uninhabited = bool_of_postcard ctx state in
     let* variant_layouts =
       index_vec_of_postcard variant_id_of_postcard variant_layout_of_postcard
         ctx state
     in
     Ok
       ({ size; align; discriminant_layout; uninhabited; variant_layouts }
         : layout))

and lifetime_mutability_of_postcard (ctx : of_postcard_ctx)
    (state : postcard_state) : (lifetime_mutability, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 -> Ok LtMutable
     | 1 -> Ok LtShared
     | 2 -> Ok LtUnknown
     | _ ->
         Error
           ("Unmatched tag " ^ string_of_int tag
          ^ " for enum lifetime_mutability"))

and literal_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (literal, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* scalar = scalar_value_of_postcard ctx state in
         Ok (VScalar scalar)
     | 1 ->
         let* float_ = float_value_of_postcard ctx state in
         Ok (VFloat float_)
     | 2 ->
         let* bool_ = bool_of_postcard ctx state in
         Ok (VBool bool_)
     | 3 ->
         let* char_ = char_of_postcard ctx state in
         Ok (VChar char_)
     | 4 ->
         let* byte_str = list_of_postcard u8_of_postcard ctx state in
         Ok (VByteStr byte_str)
     | 5 ->
         let* str = string_of_postcard ctx state in
         Ok (VStr str)
     | _ -> Error ("Unmatched tag " ^ string_of_int tag ^ " for enum literal"))

and literal_type_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (literal_type, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* int = int_ty_of_postcard ctx state in
         Ok (TInt int)
     | 1 ->
         let* u_int = u_int_ty_of_postcard ctx state in
         Ok (TUInt u_int)
     | 2 ->
         let* float_ = float_type_of_postcard ctx state in
         Ok (TFloat float_)
     | 3 -> Ok TBool
     | 4 -> Ok TChar
     | _ ->
         Error ("Unmatched tag " ^ string_of_int tag ^ " for enum literal_type"))

and loc_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (loc, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* line = usize_of_postcard ctx state in
     let* col = usize_of_postcard ctx state in
     Ok ({ line; col } : loc))

and local_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (local, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* index = local_id_of_postcard ctx state in
     let* name = option_of_postcard string_of_postcard ctx state in
     let* span = span_of_postcard ctx state in
     let* local_ty = ty_of_postcard ctx state in
     Ok ({ index; name; span; local_ty } : local))

and local_id_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (local_id, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (LocalId.id_of_postcard ctx state)

and locals_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (locals, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* arg_count = usize_of_postcard ctx state in
     let* locals =
       index_vec_of_postcard local_id_of_postcard local_of_postcard ctx state
     in
     Ok ({ arg_count; locals } : locals))

and mir_level_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (mir_level, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 -> Ok Built
     | 1 -> Ok Promoted
     | 2 -> Ok Elaborated
     | 3 -> Ok Optimized
     | _ -> Error ("Unmatched tag " ^ string_of_int tag ^ " for enum mir_level"))

and monomorphize_mut_of_postcard (ctx : of_postcard_ctx)
    (state : postcard_state) : (monomorphize_mut, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 -> Ok All
     | 1 -> Ok ExceptTypes
     | _ ->
         Error
           ("Unmatched tag " ^ string_of_int tag ^ " for enum monomorphize_mut"))

and name_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (name, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (list_of_postcard path_elem_of_postcard ctx state)

and nullop_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (nullop, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 -> Ok SizeOf
     | 1 -> Ok AlignOf
     | 2 ->
         let* x_0 = type_decl_ref_of_postcard ctx state in
         let* x_1 = option_of_postcard variant_id_of_postcard ctx state in
         let* x_2 = field_id_of_postcard ctx state in
         Ok (OffsetOf (x_0, x_1, x_2))
     | 3 -> Ok UbChecks
     | 4 -> Ok OverflowChecks
     | 5 -> Ok ContractChecks
     | _ -> Error ("Unmatched tag " ^ string_of_int tag ^ " for enum nullop"))

and operand_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (operand, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* copy = place_of_postcard ctx state in
         Ok (Copy copy)
     | 1 ->
         let* move = place_of_postcard ctx state in
         Ok (Move move)
     | 2 ->
         let* const = box_of_postcard constant_expr_of_postcard ctx state in
         Ok (Constant const)
     | _ -> Error ("Unmatched tag " ^ string_of_int tag ^ " for enum operand"))

and outlives_pred_of_postcard :
    'a0 'a1.
    (of_postcard_ctx -> postcard_state -> ('a0, string) result) ->
    (of_postcard_ctx -> postcard_state -> ('a1, string) result) ->
    of_postcard_ctx ->
    postcard_state ->
    (('a0, 'a1) outlives_pred, string) result =
 fun arg0_of_postcard arg1_of_postcard ctx state ->
  combine_postcard_error_msgs state __FUNCTION__
    (let* x_0 = arg0_of_postcard ctx state in
     let* x_1 = arg1_of_postcard ctx state in
     Ok (x_0, x_1))

and overflow_mode_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (overflow_mode, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 -> Ok OPanic
     | 1 -> Ok OUB
     | 2 -> Ok OWrap
     | _ ->
         Error ("Unmatched tag " ^ string_of_int tag ^ " for enum overflow_mode"))

and path_elem_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (path_elem, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* x_0 = string_of_postcard ctx state in
         let* x_1 = disambiguator_of_postcard ctx state in
         Ok (PeIdent (x_0, x_1))
     | 1 ->
         let* impl = impl_elem_of_postcard ctx state in
         Ok (PeImpl impl)
     | 2 ->
         let* instantiated =
           box_of_postcard
             (binder_of_postcard generic_args_of_postcard)
             ctx state
         in
         Ok (PeInstantiated instantiated)
     | _ -> Error ("Unmatched tag " ^ string_of_int tag ^ " for enum path_elem"))

and place_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (place, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* kind = place_kind_of_postcard ctx state in
     let* ty = ty_of_postcard ctx state in
     Ok ({ kind; ty } : place))

and place_kind_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (place_kind, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* local = local_id_of_postcard ctx state in
         Ok (PlaceLocal local)
     | 1 ->
         let* x_0 = box_of_postcard place_of_postcard ctx state in
         let* x_1 = projection_elem_of_postcard ctx state in
         Ok (PlaceProjection (x_0, x_1))
     | 2 ->
         let* global = global_decl_ref_of_postcard ctx state in
         Ok (PlaceGlobal global)
     | _ -> Error ("Unmatched tag " ^ string_of_int tag ^ " for enum place_kind"))

and preset_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (preset, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 -> Ok OldDefaults
     | 1 -> Ok RawMir
     | 2 -> Ok Aeneas
     | 3 -> Ok Eurydice
     | 4 -> Ok Soteria
     | 5 -> Ok Tests
     | _ -> Error ("Unmatched tag " ^ string_of_int tag ^ " for enum preset"))

and projection_elem_of_postcard (ctx : of_postcard_ctx) (state : postcard_state)
    : (projection_elem, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 -> Ok Deref
     | 1 ->
         let* x_0 = field_proj_kind_of_postcard ctx state in
         let* x_1 = field_id_of_postcard ctx state in
         Ok (Field (x_0, x_1))
     | 2 -> Ok PtrMetadata
     | 3 ->
         let* offset = box_of_postcard operand_of_postcard ctx state in
         let* from_end = bool_of_postcard ctx state in
         Ok (ProjIndex (offset, from_end))
     | 4 ->
         let* from = box_of_postcard operand_of_postcard ctx state in
         let* to_ = box_of_postcard operand_of_postcard ctx state in
         let* from_end = bool_of_postcard ctx state in
         Ok (Subslice (from, to_, from_end))
     | _ ->
         Error
           ("Unmatched tag " ^ string_of_int tag ^ " for enum projection_elem"))

and provenance_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (provenance, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* global = global_decl_ref_of_postcard ctx state in
         Ok (ProvGlobal global)
     | 1 ->
         let* function_ = fun_decl_ref_of_postcard ctx state in
         Ok (ProvFunction function_)
     | 2 -> Ok ProvUnknown
     | _ -> Error ("Unmatched tag " ^ string_of_int tag ^ " for enum provenance"))

and ptr_metadata_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (ptr_metadata, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 -> Ok NoMetadata
     | 1 -> Ok Length
     | 2 ->
         let* v_table = type_decl_ref_of_postcard ctx state in
         Ok (VTable v_table)
     | 3 ->
         let* inherit_from = ty_of_postcard ctx state in
         Ok (InheritFrom inherit_from)
     | _ ->
         Error ("Unmatched tag " ^ string_of_int tag ^ " for enum ptr_metadata"))

and raw_attribute_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (raw_attribute, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* path = string_of_postcard ctx state in
     let* args = option_of_postcard string_of_postcard ctx state in
     Ok ({ path; args } : raw_attribute))

and ref_kind_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (ref_kind, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 -> Ok RMut
     | 1 -> Ok RShared
     | _ -> Error ("Unmatched tag " ^ string_of_int tag ^ " for enum ref_kind"))

and region_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (region, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* var = de_bruijn_var_of_postcard region_id_of_postcard ctx state in
         Ok (RVar var)
     | 1 -> Ok RStatic
     | 2 ->
         let* body = region_id_of_postcard ctx state in
         Ok (RBody body)
     | 3 -> Ok RErased
     | _ -> Error ("Unmatched tag " ^ string_of_int tag ^ " for enum region"))

and region_binder_of_postcard :
    'a0.
    (of_postcard_ctx -> postcard_state -> ('a0, string) result) ->
    of_postcard_ctx ->
    postcard_state ->
    ('a0 region_binder, string) result =
 fun arg0_of_postcard ctx state ->
  combine_postcard_error_msgs state __FUNCTION__
    (let* binder_regions =
       index_map_of_postcard region_id_of_postcard region_param_of_postcard ctx
         state
     in
     let* binder_value = arg0_of_postcard ctx state in
     Ok ({ binder_regions; binder_value } : _ region_binder))

and region_id_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (region_id, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (RegionId.id_of_postcard ctx state)

and region_param_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (region_param, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* index = region_id_of_postcard ctx state in
     let* name = option_of_postcard string_of_postcard ctx state in
     let* mutability = lifetime_mutability_of_postcard ctx state in
     Ok ({ index; name; mutability } : region_param))

and repr_algorithm_of_postcard (ctx : of_postcard_ctx) (state : postcard_state)
    : (repr_algorithm, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 -> Ok Rust
     | 1 -> Ok C
     | _ ->
         Error
           ("Unmatched tag " ^ string_of_int tag ^ " for enum repr_algorithm"))

and repr_options_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (repr_options, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* repr_algo = repr_algorithm_of_postcard ctx state in
     let* align_modif =
       option_of_postcard alignment_modifier_of_postcard ctx state
     in
     let* transparent = bool_of_postcard ctx state in
     let* explicit_discr_type = bool_of_postcard ctx state in
     Ok
       ({ repr_algo; align_modif; transparent; explicit_discr_type }
         : repr_options))

and rvalue_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (rvalue, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* use = operand_of_postcard ctx state in
         Ok (Use use)
     | 1 ->
         let* place = place_of_postcard ctx state in
         let* kind = borrow_kind_of_postcard ctx state in
         let* ptr_metadata = operand_of_postcard ctx state in
         Ok (RvRef (place, kind, ptr_metadata))
     | 2 ->
         let* place = place_of_postcard ctx state in
         let* kind = ref_kind_of_postcard ctx state in
         let* ptr_metadata = operand_of_postcard ctx state in
         Ok (RawPtr (place, kind, ptr_metadata))
     | 3 ->
         let* x_0 = binop_of_postcard ctx state in
         let* x_1 = operand_of_postcard ctx state in
         let* x_2 = operand_of_postcard ctx state in
         Ok (BinaryOp (x_0, x_1, x_2))
     | 4 ->
         let* x_0 = unop_of_postcard ctx state in
         let* x_1 = operand_of_postcard ctx state in
         Ok (UnaryOp (x_0, x_1))
     | 5 ->
         let* x_0 = nullop_of_postcard ctx state in
         let* x_1 = ty_of_postcard ctx state in
         Ok (NullaryOp (x_0, x_1))
     | 6 ->
         let* discriminant = place_of_postcard ctx state in
         Ok (Discriminant discriminant)
     | 7 ->
         let* x_0 = aggregate_kind_of_postcard ctx state in
         let* x_1 = list_of_postcard operand_of_postcard ctx state in
         Ok (Aggregate (x_0, x_1))
     | 8 ->
         let* x_0 = place_of_postcard ctx state in
         let* x_1 = ty_of_postcard ctx state in
         let* x_2 =
           option_of_postcard
             (box_of_postcard constant_expr_of_postcard)
             ctx state
         in
         Ok (Len (x_0, x_1, x_2))
     | 9 ->
         let* x_0 = operand_of_postcard ctx state in
         let* x_1 = ty_of_postcard ctx state in
         let* x_2 = box_of_postcard constant_expr_of_postcard ctx state in
         Ok (Repeat (x_0, x_1, x_2))
     | 10 ->
         let* x_0 = operand_of_postcard ctx state in
         let* x_1 = ty_of_postcard ctx state in
         Ok (ShallowInitBox (x_0, x_1))
     | _ -> Error ("Unmatched tag " ^ string_of_int tag ^ " for enum rvalue"))

and scalar_value_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (scalar_value, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* x_0 = u_int_ty_of_postcard ctx state in
         let* x_1 = u128_of_postcard ctx state in
         Ok (UnsignedScalar (x_0, x_1))
     | 1 ->
         let* x_0 = int_ty_of_postcard ctx state in
         let* x_1 = i128_of_postcard ctx state in
         Ok (SignedScalar (x_0, x_1))
     | _ ->
         Error ("Unmatched tag " ^ string_of_int tag ^ " for enum scalar_value"))

and span_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (span, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* data = span_data_of_postcard ctx state in
     let* generated_from_span =
       option_of_postcard span_data_of_postcard ctx state
     in
     Ok ({ data; generated_from_span } : span))

and span_data_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (span_data, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* file = file_id_of_postcard ctx state in
     let* beg_loc = loc_of_postcard ctx state in
     let* end_loc = loc_of_postcard ctx state in
     Ok ({ file; beg_loc; end_loc } : span_data))

and tag_encoding_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (tag_encoding, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 -> Ok Direct
     | 1 ->
         let* untagged_variant = variant_id_of_postcard ctx state in
         Ok (Niche untagged_variant)
     | _ ->
         Error ("Unmatched tag " ^ string_of_int tag ^ " for enum tag_encoding"))

and target_info_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (target_info, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* target_pointer_size = u64_of_postcard ctx state in
     let* is_little_endian = bool_of_postcard ctx state in
     Ok ({ target_pointer_size; is_little_endian } : target_info))

and trait_assoc_const_of_postcard (ctx : of_postcard_ctx)
    (state : postcard_state) : (trait_assoc_const, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* name = trait_item_name_of_postcard ctx state in
     let* attr_info = attr_info_of_postcard ctx state in
     let* ty = ty_of_postcard ctx state in
     let* default = option_of_postcard global_decl_ref_of_postcard ctx state in
     Ok ({ name; attr_info; ty; default } : trait_assoc_const))

and trait_assoc_ty_of_postcard (ctx : of_postcard_ctx) (state : postcard_state)
    : (trait_assoc_ty, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* name = trait_item_name_of_postcard ctx state in
     let* attr_info = attr_info_of_postcard ctx state in
     let* default =
       option_of_postcard trait_assoc_ty_impl_of_postcard ctx state
     in
     let* implied_clauses =
       index_map_of_postcard trait_clause_id_of_postcard trait_param_of_postcard
         ctx state
     in
     Ok ({ name; attr_info; default; implied_clauses } : trait_assoc_ty))

and trait_assoc_ty_impl_of_postcard (ctx : of_postcard_ctx)
    (state : postcard_state) : (trait_assoc_ty_impl, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* value = ty_of_postcard ctx state in
     Ok ({ value } : trait_assoc_ty_impl))

and trait_clause_id_of_postcard (ctx : of_postcard_ctx) (state : postcard_state)
    : (trait_clause_id, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (TraitClauseId.id_of_postcard ctx state)

and trait_decl_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (trait_decl, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* def_id = trait_decl_id_of_postcard ctx state in
     let* item_meta = item_meta_of_postcard ctx state in
     let* generics = generic_params_of_postcard ctx state in
     let* implied_clauses =
       index_map_of_postcard trait_clause_id_of_postcard trait_param_of_postcard
         ctx state
     in
     let* consts = list_of_postcard trait_assoc_const_of_postcard ctx state in
     let* types =
       list_of_postcard
         (binder_of_postcard trait_assoc_ty_of_postcard)
         ctx state
     in
     let* methods =
       list_of_postcard (binder_of_postcard trait_method_of_postcard) ctx state
     in
     let* vtable = option_of_postcard type_decl_ref_of_postcard ctx state in
     Ok
       ({
          def_id;
          item_meta;
          generics;
          implied_clauses;
          consts;
          types;
          methods;
          vtable;
        }
         : trait_decl))

and trait_decl_id_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (trait_decl_id, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (TraitDeclId.id_of_postcard ctx state)

and trait_decl_ref_of_postcard (ctx : of_postcard_ctx) (state : postcard_state)
    : (trait_decl_ref, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* id = trait_decl_id_of_postcard ctx state in
     let* generics = box_of_postcard generic_args_of_postcard ctx state in
     Ok ({ id; generics } : trait_decl_ref))

and trait_impl_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (trait_impl, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* def_id = trait_impl_id_of_postcard ctx state in
     let* item_meta = item_meta_of_postcard ctx state in
     let* impl_trait = trait_decl_ref_of_postcard ctx state in
     let* generics = generic_params_of_postcard ctx state in
     let* implied_trait_refs =
       index_map_of_postcard trait_clause_id_of_postcard trait_ref_of_postcard
         ctx state
     in
     let* consts =
       list_of_postcard
         (pair_of_postcard trait_item_name_of_postcard
            global_decl_ref_of_postcard)
         ctx state
     in
     let* types =
       list_of_postcard
         (pair_of_postcard trait_item_name_of_postcard
            (binder_of_postcard trait_assoc_ty_impl_of_postcard))
         ctx state
     in
     let* methods =
       list_of_postcard
         (pair_of_postcard trait_item_name_of_postcard
            (binder_of_postcard fun_decl_ref_of_postcard))
         ctx state
     in
     let* vtable = option_of_postcard global_decl_ref_of_postcard ctx state in
     Ok
       ({
          def_id;
          item_meta;
          impl_trait;
          generics;
          implied_trait_refs;
          consts;
          types;
          methods;
          vtable;
        }
         : trait_impl))

and trait_impl_id_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (trait_impl_id, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (TraitImplId.id_of_postcard ctx state)

and trait_impl_ref_of_postcard (ctx : of_postcard_ctx) (state : postcard_state)
    : (trait_impl_ref, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* id = trait_impl_id_of_postcard ctx state in
     let* generics = box_of_postcard generic_args_of_postcard ctx state in
     Ok ({ id; generics } : trait_impl_ref))

and trait_item_name_of_postcard (ctx : of_postcard_ctx) (state : postcard_state)
    : (trait_item_name, string) result =
  combine_postcard_error_msgs state __FUNCTION__ (string_of_postcard ctx state)

and trait_method_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (trait_method, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* name = trait_item_name_of_postcard ctx state in
     let* attr_info = attr_info_of_postcard ctx state in
     let* item = fun_decl_ref_of_postcard ctx state in
     Ok ({ name; attr_info; item } : trait_method))

and trait_param_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (trait_param, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* clause_id = trait_clause_id_of_postcard ctx state in
     let* span = option_of_postcard span_of_postcard ctx state in
     let* trait =
       region_binder_of_postcard trait_decl_ref_of_postcard ctx state
     in
     Ok ({ clause_id; span; trait } : trait_param))

and trait_ref_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (trait_ref, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (hash_consed_val_of_postcard ctx.tref_hashcons_map
       trait_ref_contents_of_postcard ctx state)

and trait_ref_contents_of_postcard (ctx : of_postcard_ctx)
    (state : postcard_state) : (trait_ref_contents, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* kind = trait_ref_kind_of_postcard ctx state in
     let* trait_decl_ref =
       region_binder_of_postcard trait_decl_ref_of_postcard ctx state
     in
     Ok ({ kind; trait_decl_ref } : trait_ref_contents))

and trait_ref_kind_of_postcard (ctx : of_postcard_ctx) (state : postcard_state)
    : (trait_ref_kind, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* trait_impl = trait_impl_ref_of_postcard ctx state in
         Ok (TraitImpl trait_impl)
     | 1 ->
         let* clause =
           de_bruijn_var_of_postcard trait_clause_id_of_postcard ctx state
         in
         Ok (Clause clause)
     | 2 ->
         let* x_0 = box_of_postcard trait_ref_of_postcard ctx state in
         let* x_1 = trait_clause_id_of_postcard ctx state in
         Ok (ParentClause (x_0, x_1))
     | 3 ->
         let* x_0 = box_of_postcard trait_ref_of_postcard ctx state in
         let* x_1 = trait_item_name_of_postcard ctx state in
         let* x_2 = trait_clause_id_of_postcard ctx state in
         Ok (ItemClause (x_0, x_1, x_2))
     | 4 -> Ok Self
     | 5 ->
         let* builtin_data = builtin_impl_data_of_postcard ctx state in
         let* parent_trait_refs =
           index_map_of_postcard trait_clause_id_of_postcard
             trait_ref_of_postcard ctx state
         in
         let* types =
           list_of_postcard
             (pair_of_postcard trait_item_name_of_postcard
                trait_assoc_ty_impl_of_postcard)
             ctx state
         in
         Ok (BuiltinOrAuto (builtin_data, parent_trait_refs, types))
     | 6 -> Ok Dyn
     | 7 ->
         let* unknown = string_of_postcard ctx state in
         Ok (UnknownTrait unknown)
     | _ ->
         Error
           ("Unmatched tag " ^ string_of_int tag ^ " for enum trait_ref_kind"))

and trait_type_constraint_of_postcard (ctx : of_postcard_ctx)
    (state : postcard_state) : (trait_type_constraint, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* trait_ref = trait_ref_of_postcard ctx state in
     let* type_name = trait_item_name_of_postcard ctx state in
     let* ty = ty_of_postcard ctx state in
     Ok ({ trait_ref; type_name; ty } : trait_type_constraint))

and trait_type_constraint_id_of_postcard (ctx : of_postcard_ctx)
    (state : postcard_state) : (trait_type_constraint_id, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (TraitTypeConstraintId.id_of_postcard ctx state)

and ty_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (ty, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (hash_consed_val_of_postcard ctx.ty_hashcons_map ty_kind_of_postcard ctx
       state)

and ty_kind_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (ty_kind, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* adt = type_decl_ref_of_postcard ctx state in
         Ok (TAdt adt)
     | 1 ->
         let* type_var =
           de_bruijn_var_of_postcard type_var_id_of_postcard ctx state
         in
         Ok (TVar type_var)
     | 2 ->
         let* literal = literal_type_of_postcard ctx state in
         Ok (TLiteral literal)
     | 3 -> Ok TNever
     | 4 ->
         let* x_0 = region_of_postcard ctx state in
         let* x_1 = ty_of_postcard ctx state in
         let* x_2 = ref_kind_of_postcard ctx state in
         Ok (TRef (x_0, x_1, x_2))
     | 5 ->
         let* x_0 = ty_of_postcard ctx state in
         let* x_1 = ref_kind_of_postcard ctx state in
         Ok (TRawPtr (x_0, x_1))
     | 6 ->
         let* x_0 = trait_ref_of_postcard ctx state in
         let* x_1 = trait_item_name_of_postcard ctx state in
         Ok (TTraitType (x_0, x_1))
     | 7 ->
         let* dyn_trait = dyn_predicate_of_postcard ctx state in
         Ok (TDynTrait dyn_trait)
     | 8 ->
         let* fn_ptr =
           region_binder_of_postcard fun_sig_of_postcard ctx state
         in
         Ok (TFnPtr fn_ptr)
     | 9 ->
         let* fn_def = region_binder_of_postcard fn_ptr_of_postcard ctx state in
         Ok (TFnDef fn_def)
     | 10 ->
         let* ptr_metadata = ty_of_postcard ctx state in
         Ok (TPtrMetadata ptr_metadata)
     | 11 ->
         let* x_0 = ty_of_postcard ctx state in
         let* x_1 = box_of_postcard constant_expr_of_postcard ctx state in
         Ok (TArray (x_0, x_1))
     | 12 ->
         let* slice = ty_of_postcard ctx state in
         Ok (TSlice slice)
     | 13 ->
         let* error = string_of_postcard ctx state in
         Ok (TError error)
     | _ -> Error ("Unmatched tag " ^ string_of_int tag ^ " for enum ty_kind"))

and type_decl_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (type_decl, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* def_id = type_decl_id_of_postcard ctx state in
     let* item_meta = item_meta_of_postcard ctx state in
     let* generics = generic_params_of_postcard ctx state in
     let* src = item_source_of_postcard ctx state in
     let* kind = type_decl_kind_of_postcard ctx state in
     let* layout = option_of_postcard layout_of_postcard ctx state in
     let* ptr_metadata = ptr_metadata_of_postcard ctx state in
     let* repr = option_of_postcard repr_options_of_postcard ctx state in
     Ok
       ({ def_id; item_meta; generics; src; kind; layout; ptr_metadata; repr }
         : type_decl))

and type_decl_id_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (type_decl_id, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (TypeDeclId.id_of_postcard ctx state)

and type_decl_kind_of_postcard (ctx : of_postcard_ctx) (state : postcard_state)
    : (type_decl_kind, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* struct_ =
           index_vec_of_postcard field_id_of_postcard field_of_postcard ctx
             state
         in
         Ok (Struct struct_)
     | 1 ->
         let* enum =
           index_vec_of_postcard variant_id_of_postcard variant_of_postcard ctx
             state
         in
         Ok (Enum enum)
     | 2 ->
         let* union =
           index_vec_of_postcard field_id_of_postcard field_of_postcard ctx
             state
         in
         Ok (Union union)
     | 3 -> Ok Opaque
     | 4 ->
         let* alias = ty_of_postcard ctx state in
         Ok (Alias alias)
     | 5 ->
         let* error = string_of_postcard ctx state in
         Ok (TDeclError error)
     | _ ->
         Error
           ("Unmatched tag " ^ string_of_int tag ^ " for enum type_decl_kind"))

and type_decl_ref_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (type_decl_ref, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* id = type_id_of_postcard ctx state in
     let* generics = box_of_postcard generic_args_of_postcard ctx state in
     Ok ({ id; generics } : type_decl_ref))

and type_id_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (type_id, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* adt = type_decl_id_of_postcard ctx state in
         Ok (TAdtId adt)
     | 1 -> Ok TTuple
     | 2 ->
         let* builtin = builtin_ty_of_postcard ctx state in
         Ok (TBuiltin builtin)
     | _ -> Error ("Unmatched tag " ^ string_of_int tag ^ " for enum type_id"))

and type_param_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (type_param, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* index = type_var_id_of_postcard ctx state in
     let* name = string_of_postcard ctx state in
     Ok ({ index; name } : type_param))

and type_var_id_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (type_var_id, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (TypeVarId.id_of_postcard ctx state)

and u_int_ty_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (u_int_ty, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 -> Ok Usize
     | 1 -> Ok U8
     | 2 -> Ok U16
     | 3 -> Ok U32
     | 4 -> Ok U64
     | 5 -> Ok U128
     | _ -> Error ("Unmatched tag " ^ string_of_int tag ^ " for enum u_int_ty"))

and unop_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (unop, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 -> Ok Not
     | 1 ->
         let* neg = overflow_mode_of_postcard ctx state in
         Ok (Neg neg)
     | 2 ->
         let* cast = cast_kind_of_postcard ctx state in
         Ok (Cast cast)
     | _ -> Error ("Unmatched tag " ^ string_of_int tag ^ " for enum unop"))

and unsizing_metadata_of_postcard (ctx : of_postcard_ctx)
    (state : postcard_state) : (unsizing_metadata, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* length = box_of_postcard constant_expr_of_postcard ctx state in
         Ok (MetaLength length)
     | 1 ->
         let* x_0 = trait_ref_of_postcard ctx state in
         let* x_1 = box_of_postcard constant_expr_of_postcard ctx state in
         Ok (MetaVTable (x_0, x_1))
     | 2 ->
         let* v_table_upcast =
           list_of_postcard field_id_of_postcard ctx state
         in
         Ok (MetaVTableUpcast v_table_upcast)
     | 3 -> Ok MetaUnknown
     | _ ->
         Error
           ("Unmatched tag " ^ string_of_int tag ^ " for enum unsizing_metadata"))

and v_table_field_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (v_table_field, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 -> Ok VTableSize
     | 1 -> Ok VTableAlign
     | 2 -> Ok VTableDrop
     | 3 ->
         let* method_ = trait_item_name_of_postcard ctx state in
         Ok (VTableMethod method_)
     | 4 ->
         let* super_trait = trait_clause_id_of_postcard ctx state in
         Ok (VTableSuperTrait super_trait)
     | _ ->
         Error ("Unmatched tag " ^ string_of_int tag ^ " for enum v_table_field"))

and variant_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (variant, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* span = span_of_postcard ctx state in
     let* attr_info = attr_info_of_postcard ctx state in
     let* variant_name = string_of_postcard ctx state in
     let* fields =
       index_vec_of_postcard field_id_of_postcard field_of_postcard ctx state
     in
     let* discriminant = literal_of_postcard ctx state in
     Ok ({ span; attr_info; variant_name; fields; discriminant } : variant))

and variant_id_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (variant_id, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (VariantId.id_of_postcard ctx state)

and variant_layout_of_postcard (ctx : of_postcard_ctx) (state : postcard_state)
    : (variant_layout, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* field_offsets =
       index_vec_of_postcard field_id_of_postcard u64_of_postcard ctx state
     in
     let* uninhabited = bool_of_postcard ctx state in
     let* tag = option_of_postcard scalar_value_of_postcard ctx state in
     Ok ({ field_offsets; uninhabited; tag } : variant_layout))
