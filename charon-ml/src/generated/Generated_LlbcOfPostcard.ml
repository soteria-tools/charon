open OfPostcardBasic
open Types
open LlbcAst
open GAstOfPostcard

let rec ___ = ()

and block_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (block, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* span = span_of_postcard ctx state in
     let* statements = list_of_postcard statement_of_postcard ctx state in
     Ok ({ span; statements } : block))

and statement_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (statement, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* span = span_of_postcard ctx state in
     let* statement_id = statement_id_of_postcard ctx state in
     let* kind = statement_kind_of_postcard ctx state in
     let* comments_before = list_of_postcard string_of_postcard ctx state in
     Ok ({ span; statement_id; kind; comments_before } : statement))

and statement_id_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (statement_id, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (StatementId.id_of_postcard ctx state)

and statement_kind_of_postcard (ctx : of_postcard_ctx) (state : postcard_state)
    : (statement_kind, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* x_0 = place_of_postcard ctx state in
         let* x_1 = rvalue_of_postcard ctx state in
         Ok (Assign (x_0, x_1))
     | 1 ->
         let* x_0 = place_of_postcard ctx state in
         let* x_1 = variant_id_of_postcard ctx state in
         Ok (SetDiscriminant (x_0, x_1))
     | 2 ->
         let* copy_non_overlapping =
           box_of_postcard copy_non_overlapping_of_postcard ctx state
         in
         Ok (CopyNonOverlapping copy_non_overlapping)
     | 3 ->
         let* storage_live = local_id_of_postcard ctx state in
         Ok (StorageLive storage_live)
     | 4 ->
         let* storage_dead = local_id_of_postcard ctx state in
         Ok (StorageDead storage_dead)
     | 5 ->
         let* place_mention = place_of_postcard ctx state in
         Ok (PlaceMention place_mention)
     | 6 ->
         let* x_0 = place_of_postcard ctx state in
         let* x_1 = trait_ref_of_postcard ctx state in
         let* x_2 = drop_kind_of_postcard ctx state in
         Ok (Drop (x_0, x_1, x_2))
     | 7 ->
         let* assert_ = assertion_of_postcard ctx state in
         let* on_failure = abort_kind_of_postcard ctx state in
         Ok (Assert (assert_, on_failure))
     | 8 ->
         let* call = call_of_postcard ctx state in
         Ok (Call call)
     | 9 ->
         let* abort = abort_kind_of_postcard ctx state in
         Ok (Abort abort)
     | 10 -> Ok Return
     | 11 ->
         let* break = usize_of_postcard ctx state in
         Ok (Break break)
     | 12 ->
         let* continue = usize_of_postcard ctx state in
         Ok (Continue continue)
     | 13 -> Ok Nop
     | 14 ->
         let* switch = switch_of_postcard ctx state in
         Ok (Switch switch)
     | 15 ->
         let* loop = block_of_postcard ctx state in
         Ok (Loop loop)
     | 16 ->
         let* error = string_of_postcard ctx state in
         Ok (Error error)
     | _ ->
         Error
           ("Unmatched tag " ^ string_of_int tag ^ " for enum statement_kind"))

and switch_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (switch, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* x_0 = operand_of_postcard ctx state in
         let* x_1 = block_of_postcard ctx state in
         let* x_2 = block_of_postcard ctx state in
         Ok (If (x_0, x_1, x_2))
     | 1 ->
         let* x_0 = operand_of_postcard ctx state in
         let* x_1 = literal_type_of_postcard ctx state in
         let* x_2 =
           list_of_postcard
             (pair_of_postcard
                (list_of_postcard literal_of_postcard)
                block_of_postcard)
             ctx state
         in
         let* x_3 = block_of_postcard ctx state in
         Ok (SwitchInt (x_0, x_1, x_2, x_3))
     | 2 ->
         let* x_0 = place_of_postcard ctx state in
         let* x_1 =
           list_of_postcard
             (pair_of_postcard
                (list_of_postcard variant_id_of_postcard)
                block_of_postcard)
             ctx state
         in
         let* x_2 = option_of_postcard block_of_postcard ctx state in
         Ok (Match (x_0, x_1, x_2))
     | _ -> Error ("Unmatched tag " ^ string_of_int tag ^ " for enum switch"))
