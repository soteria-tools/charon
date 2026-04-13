open OfPostcardBasic
open Types
open Expressions
open UllbcAst
open GAstOfPostcard

let rec ___ = ()

and block_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (block, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* statements = list_of_postcard statement_of_postcard ctx state in
     let* terminator = terminator_of_postcard ctx state in
     Ok ({ statements; terminator } : block))

and block_id_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (block_id, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (BlockId.id_of_postcard ctx state)

and statement_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (statement, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* span = span_of_postcard ctx state in
     let* kind = statement_kind_of_postcard ctx state in
     let* comments_before = list_of_postcard string_of_postcard ctx state in
     Ok ({ span; kind; comments_before } : statement))

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
         let* assert_ = assertion_of_postcard ctx state in
         let* on_failure = abort_kind_of_postcard ctx state in
         Ok (Assert (assert_, on_failure))
     | 7 -> Ok Nop
     | _ ->
         Error
           ("Unmatched tag " ^ string_of_int tag ^ " for enum statement_kind"))

and switch_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (switch, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* x_0 = block_id_of_postcard ctx state in
         let* x_1 = block_id_of_postcard ctx state in
         Ok (If (x_0, x_1))
     | 1 ->
         let* x_0 = literal_type_of_postcard ctx state in
         let* x_1 =
           list_of_postcard
             (pair_of_postcard literal_of_postcard block_id_of_postcard)
             ctx state
         in
         let* x_2 = block_id_of_postcard ctx state in
         Ok (SwitchInt (x_0, x_1, x_2))
     | _ -> Error ("Unmatched tag " ^ string_of_int tag ^ " for enum switch"))

and terminator_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (terminator, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* span = span_of_postcard ctx state in
     let* kind = terminator_kind_of_postcard ctx state in
     let* comments_before = list_of_postcard string_of_postcard ctx state in
     Ok ({ span; kind; comments_before } : terminator))

and terminator_kind_of_postcard (ctx : of_postcard_ctx) (state : postcard_state)
    : (terminator_kind, string) result =
  combine_postcard_error_msgs state __FUNCTION__
    (let* tag = decode_varint_u32 state in
     match tag with
     | 0 ->
         let* target = block_id_of_postcard ctx state in
         Ok (Goto target)
     | 1 ->
         let* discr = operand_of_postcard ctx state in
         let* targets = switch_of_postcard ctx state in
         Ok (Switch (discr, targets))
     | 2 ->
         let* call = call_of_postcard ctx state in
         let* target = block_id_of_postcard ctx state in
         let* on_unwind = block_id_of_postcard ctx state in
         Ok (Call (call, target, on_unwind))
     | 3 ->
         let* kind = drop_kind_of_postcard ctx state in
         let* place = place_of_postcard ctx state in
         let* tref = trait_ref_of_postcard ctx state in
         let* target = block_id_of_postcard ctx state in
         let* on_unwind = block_id_of_postcard ctx state in
         Ok (Drop (kind, place, tref, target, on_unwind))
     | 4 ->
         let* assert_ = assertion_of_postcard ctx state in
         let* target = block_id_of_postcard ctx state in
         let* on_unwind = block_id_of_postcard ctx state in
         Ok (TAssert (assert_, target, on_unwind))
     | 5 ->
         let* abort = abort_kind_of_postcard ctx state in
         Ok (Abort abort)
     | 6 -> Ok Return
     | 7 -> Ok UnwindResume
     | _ ->
         Error
           ("Unmatched tag " ^ string_of_int tag ^ " for enum terminator_kind"))
