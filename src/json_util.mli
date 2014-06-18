exception Conv_exception of Json_conv.target Meta_conv.Error.t

type 'a decoder = ('a, Tiny_json.Json.t) Meta_conv.Types.Decoder.t

type 'a encoder = ('a, Tiny_json.Json.t) Meta_conv.Types.Encoder.t

val x_of_json_string : (string -> 'a) -> 'a decoder

val x_of_json_int : (int -> 'a) -> 'a decoder

val to_json_string : ('a encoder) -> 'a -> string