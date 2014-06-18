open Core.Std

exception Conv_exception of Json_conv.target Meta_conv.Error.t

type 'a decoder = ('a, Tiny_json.Json.t) Meta_conv.Types.Decoder.t

type 'a encoder = ('a, Tiny_json.Json.t) Meta_conv.Types.Encoder.t

let x_of_json_string converter ?trace j =
  let err type' = 
    let t = match trace with | Some t -> t | None -> [] in
    let msg = Printf.sprintf "Expected string, found %s" type' in
    `Error (Meta_conv.Error.Primitive_decoding_failure msg, j, t)
  in
  match j with 
  | Tiny_json.Json.String s -> `Ok (converter s)
  | Tiny_json.Json.Number _ -> err "number"
  | Tiny_json.Json.Array _ -> err "array"
  | Tiny_json.Json.Bool _ -> err "bool"
  | Tiny_json.Json.Null -> err "null"
  | Tiny_json.Json.Object o -> err "object"

let x_of_json_int converter ?trace j =
  let err type' = 
    let t = match trace with | Some t -> t | None -> [] in
    let msg = Printf.sprintf "Expected int, found %s" type' in
    `Error (Meta_conv.Error.Primitive_decoding_failure msg, j, t)
  in
  match j with 
  | Tiny_json.Json.String _ -> err "string"
  | Tiny_json.Json.Number i -> `Ok (Int.of_string i |> converter)
  | Tiny_json.Json.Array _ -> err "array"
  | Tiny_json.Json.Bool _ -> err "bool"
  | Tiny_json.Json.Null -> err "null"
  | Tiny_json.Json.Object o -> err "object"

let to_json_string encoder t = 
  let buf = Buffer.create 0 in
  let formatter = Format.formatter_of_buffer buf in
  encoder t |> Tiny_json.Json.format formatter;
  Format.pp_print_flush formatter ();
  let json = Buffer.contents buf in
  Buffer.clear buf;
  json