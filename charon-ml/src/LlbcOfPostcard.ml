open OfPostcardBasic
open Types
open LlbcAst
include GAstOfPostcard
include Generated_LlbcOfPostcard

let expr_body_of_postcard (ctx : of_postcard_ctx) (state : postcard_state) :
    (expr_body option, string) result =
  let* tag = decode_varint_u32 state in
  match tag with
  | 1 ->
      let* body = gexpr_body_of_postcard block_of_postcard ctx state in
      Ok (Some body)
  | 2 | 3 | 4 | 5 -> Ok None
  | _ -> Error "invalid body tag"

let crate_of_postcard (bytes : bytes) : (crate, string) result =
  let state = { bytes; offset = 0 } in
  gcrate_of_postcard expr_body_of_postcard state

let crate_of_postcard_file (file : string) : (crate, string) result =
  crate_of_postcard
    (Stdlib.In_channel.with_open_bin file Stdlib.In_channel.input_all
    |> Bytes.of_string)
