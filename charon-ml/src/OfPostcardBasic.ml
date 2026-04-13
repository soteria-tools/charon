type postcard_state = { bytes : bytes; mutable offset : int }

let ( let* ) o f =
  match o with
  | Error e -> Error e
  | Ok x -> f x

let show_remaining (state : postcard_state) =
  let rem = Bytes.length state.bytes - state.offset in
  if rem <= 0 then "<eof>" else Printf.sprintf "%d bytes remaining" rem

let combine_postcard_error_msgs (state : postcard_state) (msg : string)
    (res : ('a, string) result) : ('a, string) result =
  match res with
  | Ok x -> Ok x
  | Error e ->
      Error
        ("[" ^ msg ^ "] failed at offset " ^ string_of_int state.offset ^ ": "
       ^ e)

let ensure_available (state : postcard_state) (n : int) : (unit, string) result
    =
  if state.offset + n <= Bytes.length state.bytes then Ok ()
  else Error ("unexpected EOF (" ^ show_remaining state ^ ")")

let take_u8 (state : postcard_state) : (int, string) result =
  let* () = ensure_available state 1 in
  let b = Char.code (Bytes.get state.bytes state.offset) in
  state.offset <- state.offset + 1;
  Ok b

let bool_of_postcard (_ : 'ctx) (state : postcard_state) : (bool, string) result
    =
  let* b = take_u8 state in
  match b with
  | 0 -> Ok false
  | 1 -> Ok true
  | _ -> Error "invalid bool"

let decode_varint_unsigned (max_bytes : int) (state : postcard_state) :
    (int64, string) result =
  let rec loop i shift acc =
    if i >= max_bytes then Error "varint too long"
    else
      let* byte = take_u8 state in
      let payload = Int64.of_int (byte land 0x7F) in
      let acc = Int64.logor acc (Int64.shift_left payload shift) in
      if byte land 0x80 = 0 then Ok acc else loop (i + 1) (shift + 7) acc
  in
  loop 0 0 0L

let zigzag_decode_i64 (u : int64) : int64 =
  Int64.logxor (Int64.shift_right_logical u 1) (Int64.neg (Int64.logand u 1L))

let decode_varint_u16 state =
  let* v = decode_varint_unsigned 3 state in
  if Int64.compare v 0xFFFFL <= 0 then Ok (Int64.to_int v)
  else Error "u16 overflow"

let decode_varint_u32 state =
  let* v = decode_varint_unsigned 5 state in
  if Int64.compare v 0xFFFF_FFFFL <= 0 then Ok (Int64.to_int v)
  else Error "u32 overflow"

let decode_varint_u64 state = decode_varint_unsigned 10 state

let decode_varint_usize state =
  let* v = decode_varint_u64 state in
  if Int64.compare v (Int64.of_int max_int) <= 0 then Ok (Int64.to_int v)
  else Error "usize overflow"

let decode_varint_i16 state =
  let* v = decode_varint_u16 state in
  let i = Int64.to_int (zigzag_decode_i64 (Int64.of_int v)) in
  if i >= -32768 && i <= 32767 then Ok i else Error "i16 overflow"

let decode_varint_i32 state =
  let* v = decode_varint_u32 state in
  let i = zigzag_decode_i64 (Int64.of_int v) in
  if Int64.compare i (-2147483648L) >= 0 && Int64.compare i 2147483647L <= 0
  then Ok (Int64.to_int i)
  else Error "i32 overflow"

let decode_varint_i64 state =
  let* v = decode_varint_u64 state in
  Ok (zigzag_decode_i64 v)

let decode_varint_isize state =
  let* i = decode_varint_i64 state in
  if
    Int64.compare i (Int64.of_int min_int) >= 0
    && Int64.compare i (Int64.of_int max_int) <= 0
  then Ok (Int64.to_int i)
  else Error "isize overflow"

let u8_of_postcard _ state = take_u8 state

let i8_of_postcard _ state =
  let* b = take_u8 state in
  Ok (if b land 0x80 = 0 then b else b - 0x100)

let u16_of_postcard _ state = decode_varint_u16 state
let i16_of_postcard _ state = decode_varint_i16 state
let u32_of_postcard _ state = decode_varint_u32 state
let i32_of_postcard _ state = decode_varint_i32 state

let u64_of_postcard (_ : 'ctx) (state : postcard_state) : (int, string) result =
  let* v = decode_varint_u64 state in
  if Int64.compare v (Int64.of_int max_int) <= 0 then Ok (Int64.to_int v)
  else Error "u64 overflow"

let i64_of_postcard (_ : 'ctx) (state : postcard_state) : (int, string) result =
  let* i = decode_varint_i64 state in
  if
    Int64.compare i (Int64.of_int min_int) >= 0
    && Int64.compare i (Int64.of_int max_int) <= 0
  then Ok (Int64.to_int i)
  else Error "i64 overflow"

let usize_of_postcard _ state = decode_varint_usize state
let isize_of_postcard _ state = decode_varint_isize state

let int_of_postcard (_ : 'ctx) (state : postcard_state) : (int, string) result =
  decode_varint_usize state

let u128_of_postcard (_ : 'ctx) (state : postcard_state) : (Z.t, string) result
    =
  let rec loop i shift acc =
    if i >= 19 then Error "u128 varint too long"
    else
      let* byte = take_u8 state in
      let payload = Z.of_int (byte land 0x7F) in
      let acc = Z.logor acc (Z.shift_left payload shift) in
      if byte land 0x80 = 0 then Ok acc else loop (i + 1) (shift + 7) acc
  in
  loop 0 0 Z.zero

let i128_of_postcard (_ : 'ctx) (state : postcard_state) : (Z.t, string) result
    =
  let* u = u128_of_postcard () state in
  let one = Z.one in
  let sign = Z.logand u one in
  let abs = Z.shift_right_trunc u 1 in
  if Z.equal sign Z.zero then Ok abs else Ok (Z.neg (Z.succ abs))

let f32_of_postcard (_ : 'ctx) (state : postcard_state) : (float, string) result
    =
  let* () = ensure_available state 4 in
  let b0 = Char.code (Bytes.get state.bytes state.offset) in
  let b1 = Char.code (Bytes.get state.bytes (state.offset + 1)) in
  let b2 = Char.code (Bytes.get state.bytes (state.offset + 2)) in
  let b3 = Char.code (Bytes.get state.bytes (state.offset + 3)) in
  state.offset <- state.offset + 4;
  let bits =
    Int32.logor (Int32.of_int b0)
      (Int32.logor
         (Int32.shift_left (Int32.of_int b1) 8)
         (Int32.logor
            (Int32.shift_left (Int32.of_int b2) 16)
            (Int32.shift_left (Int32.of_int b3) 24)))
  in
  Ok (Int32.float_of_bits bits)

let f64_of_postcard (_ : 'ctx) (state : postcard_state) : (float, string) result
    =
  let* () = ensure_available state 8 in
  let open Int64 in
  let get i = of_int (Char.code (Bytes.get state.bytes (state.offset + i))) in
  let bits =
    logor (get 0)
      (logor
         (shift_left (get 1) 8)
         (logor
            (shift_left (get 2) 16)
            (logor
               (shift_left (get 3) 24)
               (logor
                  (shift_left (get 4) 32)
                  (logor
                     (shift_left (get 5) 40)
                     (logor (shift_left (get 6) 48) (shift_left (get 7) 56)))))))
  in
  state.offset <- state.offset + 8;
  Ok (Int64.float_of_bits bits)

let string_of_postcard (_ : 'ctx) (state : postcard_state) :
    (string, string) result =
  let* len = decode_varint_usize state in
  let* () = ensure_available state len in
  let s = Bytes.sub_string state.bytes state.offset len in
  print_string ("Read string of length " ^ string_of_int len ^ ": " ^ s ^ "\n");
  state.offset <- state.offset + len;
  Ok s

let char_of_postcard (_ : 'ctx) (state : postcard_state) :
    (Uchar.t, string) result =
  let* s = string_of_postcard () state in
  if String.length s > 4 then Error "char too long"
  else
    let u = String.get_utf_8_uchar s 0 in
    if Uchar.utf_decode_is_valid u then Ok (Uchar.utf_decode_uchar u)
    else Error "invalid utf-8 char"

let rec list_of_postcard
    (a_of_postcard : 'ctx -> postcard_state -> ('a, string) result) (ctx : 'ctx)
    (state : postcard_state) : ('a list, string) result =
  let* len = decode_varint_usize state in
  print_string ("Reading list of length " ^ string_of_int len ^ "\n");
  let rec aux i acc =
    if i = len then Ok (List.rev acc)
    else
      let* x = a_of_postcard ctx state in
      aux (i + 1) (x :: acc)
  in
  aux 0 []

let option_of_postcard
    (a_of_postcard : 'ctx -> postcard_state -> ('a, string) result) (ctx : 'ctx)
    (state : postcard_state) : ('a option, string) result =
  let* tag = take_u8 state in
  match tag with
  | 0 -> Ok None
  | 1 ->
      let* x = a_of_postcard ctx state in
      Ok (Some x)
  | _ -> Error "invalid option tag"

let pair_of_postcard a_of_postcard b_of_postcard ctx state =
  let* a = a_of_postcard ctx state in
  let* b = b_of_postcard ctx state in
  Ok (a, b)

let triple_of_postcard a_of_postcard b_of_postcard c_of_postcard ctx state =
  let* a = a_of_postcard ctx state in
  let* b = b_of_postcard ctx state in
  let* c = c_of_postcard ctx state in
  Ok (a, b, c)

let box_of_postcard a_of_postcard ctx state = a_of_postcard ctx state

let key_value_pair_of_postcard a_of_postcard b_of_postcard ctx state =
  let* a = a_of_postcard ctx state in
  let* b = b_of_postcard ctx state in
  Ok (a, b)

let tuple_0_of_postcard (_ctx : 'ctx) (_state : postcard_state) :
    (unit, string) result =
  Ok ()

let tuple_1_of_postcard a0_of_postcard ctx state =
  let* x0 = a0_of_postcard ctx state in
  Ok x0

let tuple_4_of_postcard a0_of_postcard a1_of_postcard a2_of_postcard
    a3_of_postcard ctx state =
  let* x0 = a0_of_postcard ctx state in
  let* x1 = a1_of_postcard ctx state in
  let* x2 = a2_of_postcard ctx state in
  let* x3 = a3_of_postcard ctx state in
  Ok (x0, x1, x2, x3)

let tuple_5_of_postcard a0_of_postcard a1_of_postcard a2_of_postcard
    a3_of_postcard a4_of_postcard ctx state =
  let* x0 = a0_of_postcard ctx state in
  let* x1 = a1_of_postcard ctx state in
  let* x2 = a2_of_postcard ctx state in
  let* x3 = a3_of_postcard ctx state in
  let* x4 = a4_of_postcard ctx state in
  Ok (x0, x1, x2, x3, x4)

let unit_of_postcard (_ : 'ctx) (_ : postcard_state) : (unit, string) result =
  Ok ()
