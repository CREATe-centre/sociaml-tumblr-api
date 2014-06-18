open Meta_conv.Open
open Json_conv
open Tiny_json

module Nullable(Cf : 
    sig 
      type t with conv(json)
      val null : t
    end) = struct
  
  type t = Cf.t
  
  let t_of_json ?trace j =
    match j with
    | Tiny_json.Json.Null -> `Ok Cf.null 
    | _ -> Cf.t_of_json ?trace j
  
  let json_of_t = Cf.json_of_t
  
end

module Nullable_string = Nullable(
    struct 
      type t = string with conv(json)
      let null = "" 
    end)

module Lookup = struct
  
  open Core.Std
  
  type t = {
    keys : string list;
    lookup : (string -> string option);
  }

  let t_of_json ?trace j =
    let err type' = 
      let t = match trace with | Some t -> t | None -> [] in
      let msg = Printf.sprintf "Expected object, found %s" type' in
      `Error (Meta_conv.Error.Primitive_decoding_failure msg, j, t)
      in
    match j with 
    | Tiny_json.Json.String _ -> err "string"    
    | Tiny_json.Json.Number _ -> err "number"
    | Tiny_json.Json.Array _ -> err "array"
    | Tiny_json.Json.Bool _ -> err "bool"
    | Tiny_json.Json.Null -> err "null"
    | Tiny_json.Json.Object o ->
      try
        (`Ok (o |> List.map 
        ~f:(fun (k, v) -> k, Json.show v)
        |> (fun l -> { 
          keys = List.unzip l |> (fun (a,b) -> a);
          lookup = (fun v -> List.Assoc.find l v)
        })))
      with Json_util.Conv_exception e -> `Error e 
  
  let json_of_t l = 
    Tiny_json.Json.Object (l.keys |> List.map ~f:(fun key -> key, (
          match l.lookup key with 
          | Some v -> Tiny_json.Json.String (v)
          | None -> Tiny_json.Json.Null)))

end

module Calendar = struct
  
  open CalendarLib
  open Core.Std
  
  type t = Calendar.t
  
  let t_of_json ?trace j = 
    match j with 
    | Tiny_json.Json.String _ ->
      Json_util.x_of_json_string (fun s -> Float.of_string s |> Calendar.from_unixfloat) ?trace j
    | _ -> 
      Json_util.x_of_json_int (fun i -> Float.of_int i |> Calendar.from_unixfloat) ?trace j
  
  let json_of_t t = Json.Number (Calendar.to_unixfloat t |> Int.of_float |> Int.to_string)
  
end

module Bool_YN = struct
  
  type t = bool
  
  let t_of_json = Json_util.x_of_json_string (function 
    | "Y" | "y" -> true
    | _ -> false)

  let json_of_t t = Json.String (match t with | true -> "Y" | false -> "N")
  
end

module Post_format = struct
  
  type t =
    | HTML as "html"
    | Markdown as "markdown"
    | Raw as "raw"
  with conv(json)
  
end

module Privacy = struct
  
  type t =
    | Public as "public"
    | Private as "private"
  with conv(json)
  
end

module Uri = struct
  
  type t = Uri.t
  
  let t_of_json = Json_util.x_of_json_string Uri.of_string

  let json_of_t t = Json.String (Uri.to_string t)
  
end

module Photo = struct
  
  type link = {
    width  : int;
    height : int;
    url    : Uri.t;  
  } with conv(json)
  
  type t = {
    caption       : string;
    alt_sizes     : link list;
    original_size : link;
    exif          : Lookup.t mc_option;
  } with conv(json)
  
end

module State = struct
  
  type t =
    | Published as "published"
    | Queued as "queued"
    | Draft as "draft"
    | Private as "private"
  with conv(json)
  
end

module Auto_Tweet = struct
  
  type t =
    | Off
    | On of string

end

module Post_type = struct
  
  type t =
    | Text as "text"
    | Quote as "quote"
    | Link as "link"
    | Answer as "answer"
    | Video as "video"
    | Audio as "audio"
    | Photo as "photo"
    | Chat as "chat"
  with conv(json)
  
end 

module Embedded_object = struct
  
  type t = {
    width      : int;
    embed_code : string;
  } with conv(json)
  
end

module Thumbnail = struct
  
  type dimension (: one_of :) =
    | Bool of bool
    | Int of int
  with conv(json)
  
  type uri (: one_of :) =
    | Bool of bool
    | Uri of Uri.t
  with conv(json)

end

module Note = struct
  
  type type' =
    | Like as "like"
    | Reblog as "reblog"
  with conv(json)
  
  type t = {
    timestamp       : Calendar.t;
    blog_name       : string;
    blog_url        : Uri.t;
    type' as "type" : type';
    post_id         : string mc_option;
    added_text      : string mc_option;
  } with conv(json)
  
end

module Post = struct
  
  type t = {
    blog_name              : string;
    body                   : string mc_option;
    can_reply              : bool;
    caption                : string mc_option;
    date                   : string;
    featured_in_tag        : string list mc_option;
    featured_timestamp     : Calendar.t mc_option;
    followed               : bool;
    format                 : Post_format.t;
    highlighted            : string list;
    html5_capable          : bool mc_option;
    id                     : int;
    image_permalink        : Uri.t mc_option;
    liked                  : bool;
    link_url               : Uri.t mc_option;
    notes                  : Note.t list mc_option;
    note_count             : int mc_option;
    permalink_url          : Uri.t mc_option;
    photos                 : Photo.t list mc_option;
    photoset_layout        : string mc_option;
    player                 : Embedded_object.t list mc_option;
    post_url               : Uri.t;
    reblogged_from_id      : string mc_option;
    reblogged_from_name    : string mc_option;
    reblogged_from_title   : string mc_option;
    reblogged_from_url     : Uri.t mc_option;
    reblogged_root_id      : string mc_option;
    reblogged_root_name    : string mc_option;
    reblogged_root_title   : string mc_option;
    reblogged_root_url     : Uri.t mc_option;
    reblog_key             : string;
    scheduled_publish_time : Calendar.t mc_option;
    short_url              : Uri.t;
    slug                   : string;
    source                 : string mc_option;
    source_title           : string mc_option;
    source_url             : Uri.t mc_option;
    state                  : State.t;
    tags                   : string list;
    text                   : string mc_option;
    thumbnail_height       : Thumbnail.dimension mc_option;
    thumbnail_url          : Thumbnail.uri mc_option;
    thumbnail_width        : Thumbnail.dimension mc_option;
    timestamp              : Calendar.t;
    title                  : Nullable_string.t mc_option;
    type' as "type"        : Post_type.t;
    video_type             : string mc_option;
  } with conv(json)
  
end

module Blog = struct
  
  type t = {
    admin                       : bool mc_option;
    ask                         : bool mc_option;
    ask_anon                    : bool mc_option;
    ask_page_title              : string mc_option;
    can_send_fan_mail           : bool mc_option;
    description                 : string;
    drafts                      : int mc_option;
    facebook                    : Bool_YN.t mc_option;
    facebook_opengraph_enabled  : Bool_YN.t mc_option;
    followed                    : bool mc_option;
    followers                   : int mc_option;
    is_nsfw                     : bool mc_option;
    likes                       : int mc_option;
    name                        : string;
    messages                    : int mc_option;
    posts                       : int mc_option;
    primary                     : bool mc_option;
    queue                       : int mc_option;
    share_likes                 : bool mc_option;
    title                       : string;
    type' as "type"             : Privacy.t mc_option;
    tweet                       : Bool_YN.t mc_option;
    updated                     : Calendar.t; 
    url                         : Uri.t;
  } with conv(json)
  
end

module User = struct
    
  type t = {
    blogs                : Blog.t list;
    default_post_format  : Post_format.t;
    following            : int;
    likes                : int;
    name                 : string;
  } with conv(json)

end