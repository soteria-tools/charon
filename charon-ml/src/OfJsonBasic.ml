(** This module defines various basic utilities for json deserialization. *)

open Yojson.Basic

type json = t

let ( let* ) o f =
  match o with
  | Error e -> Error e
  | Ok x -> f x

let pp_js = Yojson.Basic.pretty_print ~std:true

let combine_error_msgs (js : json) (msg : string) (res : ('a, string) result) :
    ('a, string) result =
  match res with
  | Ok x -> Ok x
  | Error e ->
      Error (Format.asprintf "[%s] failed on:@.%a\n\n%s" msg pp_js js e)

let bool_of_json (ctx : 'ctx) (js : json) : (bool, string) result =
  match js with
  | `Bool b -> Ok b
  | _ -> Error (Format.asprintf "bool_of_json: not a bool: %a" pp_js js)

let int_of_json (ctx : 'ctx) (js : json) : (int, string) result =
  match js with
  | `Int i -> Ok i
  | _ -> Error (Format.asprintf "int_of_json: not an int: %a" pp_js js)

let char_of_json (ctx : 'ctx) (js : json) : (Uchar.t, string) result =
  match js with
  | `String c ->
      if String.length c > 4 then
        Error
          (Format.asprintf "char_of_json: more than four chars: %a" pp_js js)
      else
        let uchar = String.get_utf_8_uchar c 0 in
        if Uchar.utf_decode_is_valid uchar then
          Ok (Uchar.utf_decode_uchar uchar)
        else
          Error
            (Format.asprintf "char_of_json: invalid UTF-8 char: %a" pp_js js)
  | _ -> Error (Format.asprintf "char_of_json: not a string: %a" pp_js js)

let rec of_json_list (a_of_json : 'ctx -> json -> ('a, string) result)
    (ctx : 'ctx) (jsl : json list) : ('a list, string) result =
  match jsl with
  | [] -> Ok []
  | x :: jsl' ->
      let* x = a_of_json ctx x in
      let* jsl' = of_json_list a_of_json ctx jsl' in
      Ok (x :: jsl')

let pair_of_json (a_of_json : 'ctx -> json -> ('a, string) result)
    (b_of_json : 'ctx -> json -> ('b, string) result) (ctx : 'ctx) (js : json) :
    ('a * 'b, string) result =
  match js with
  | `List [ a; b ] ->
      let* a = a_of_json ctx a in
      let* b = b_of_json ctx b in
      Ok (a, b)
  | _ -> Error (Format.asprintf "pair_of_json failed on: %a" pp_js js)

let triple_of_json (a_of_json : 'ctx -> json -> ('a, string) result)
    (b_of_json : 'ctx -> json -> ('b, string) result)
    (c_of_json : 'ctx -> json -> ('c, string) result) (ctx : 'ctx) (js : json) :
    ('a * 'b * 'c, string) result =
  match js with
  | `List [ a; b; c ] ->
      let* a = a_of_json ctx a in
      let* b = b_of_json ctx b in
      let* c = c_of_json ctx c in
      Ok (a, b, c)
  | _ -> Error (Format.asprintf "triple_of_json failed on: %a" pp_js js)

let list_of_json (a_of_json : 'ctx -> json -> ('a, string) result) (ctx : 'ctx)
    (js : json) : ('a list, string) result =
  combine_error_msgs js "list_of_json"
    (match js with
    | `List jsl -> of_json_list a_of_json ctx jsl
    | _ -> Error (Format.asprintf "list_of_json: not a list: %a" pp_js js))

let string_of_json (ctx : 'ctx) (js : json) : (string, string) result =
  match js with
  | `String str -> Ok str
  | _ -> Error (Format.asprintf "string_of_json: not a string: %a" pp_js js)

let option_of_json (a_of_json : 'ctx -> json -> ('a, string) result)
    (ctx : 'ctx) (js : json) : ('a option, string) result =
  combine_error_msgs js "option_of_json"
    (match js with
    | `Null -> Ok None
    | _ ->
        let* x = a_of_json ctx js in
        Ok (Some x))

let box_of_json (a_of_json : 'ctx -> json -> ('a, string) result) (ctx : 'ctx)
    (js : json) : ('a, string) result =
  a_of_json ctx js

let string_option_of_json (ctx : 'ctx) (js : json) :
    (string option, string) result =
  option_of_json string_of_json ctx js

let key_value_pair_of_json (a_of_json : 'ctx -> json -> ('a, string) result)
    (b_of_json : 'ctx -> json -> ('b, string) result) (ctx : 'ctx) (js : json) :
    ('a * 'b, string) result =
  match js with
  | `Assoc [ ("key", a); ("value", b) ] ->
      let* a = a_of_json ctx a in
      let* b = b_of_json ctx b in
      Ok (a, b)
  | _ -> Error (Format.asprintf "key_value_pair_of_json failed on: %a" pp_js js)
