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
    (of_postcard :
      of_postcard_ctx -> postcard_state -> ('a, string) result)
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
                 "Hash-consing key not found; there is a serialization mismatch \
                  between Rust and OCaml"
         end
     | 2 -> of_postcard ctx state
     | _ -> Error "")

let path_buf_of_postcard = string_of_postcard

let big_int_of_postcard _ (state : postcard_state) : (big_int, string) result =
  i128_of_postcard () state

(* __REPLACE0__ *)
