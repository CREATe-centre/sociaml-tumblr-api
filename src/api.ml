module type Endpoint = sig
  
  type t
  
  type meta = {
    status : int;
    msg : string;  
  }
      
  type response_envelope = {
    meta : meta;
    response : t;
  }
    
end

module type S = sig
  
  module Client : Sociaml_oauth_client_v1_0a.Client.S
  
  type error = 
    | Parameter_error of string * string
    | Conversion_error of Tiny_json.Json.t Meta_conv.Error.t
    | Client_error of Client.error

  type filter =
    | Text
    | Raw

  module Blog : sig
    
    module Info : sig
      
      type response = {
        blog : Types.Blog.t;
      }
      
      include Endpoint with type t := response
    
      val fetch : 
          basename : string ->  
          (response_envelope, error) Core_kernel.Result.t Lwt.t
    
    end
    
    module Avatar : sig
      
      type response = {
        avatar_url : Types.Uri.t;
      }
      
      type size =
        | Size_16
        | Size_24
        | Size_30
        | Size_40
        | Size_48
        | Size_64
        | Size_96
        | Size_128
        | Size_512
      
      include Endpoint with type t := response
      
      val fetch : 
          ?size : size ->
          basename : string ->
          unit ->
          (response_envelope, error) Core_kernel.Result.t Lwt.t
      
    end
    
    module Likes : sig
      
      type response = {
        liked_posts : Types.Post.t list;
        liked_count : int;
      }
      
      include Endpoint with type t := response
    
      val fetch : 
          ?limit : int -> 
          ?offset : int -> 
          basename : string ->
          unit ->  
          (response_envelope, error) Core_kernel.Result.t Lwt.t
    
    end
    
    module Followers : sig
      
      type response = {
        total_users : int;
        users : Types.User.t list;
        name : string option;
        following : bool option;
        url : Types.Uri.t option;
        updated : Types.Calendar.t option;
      }
      
      include Endpoint with type t := response
    
      val fetch : 
          ?limit : int -> 
          ?offset : int -> 
          basename : string ->
          unit ->  
          (response_envelope, error) Core_kernel.Result.t Lwt.t
    
    end
    
    module Posts : sig
      
      module Queue : sig
      
        type response = {
          posts : Types.Post.t list;
        }
        
        include Endpoint with type t := response
      
        val fetch :
            ?limit : int -> 
            ?offset : int ->
            ?filter : filter ->
            basename : string ->
            unit ->  
            (response_envelope, error) Core_kernel.Result.t Lwt.t
      
      end
      
      module Draft : sig
      
        type response = {
          posts : Types.Post.t list;
        }
        
        include Endpoint with type t := response
      
        val fetch :
            ?before_id : int -> 
            ?filter : filter ->
            basename : string ->
            unit ->  
            (response_envelope, error) Core_kernel.Result.t Lwt.t
      
      end
      
      module Submission : sig
      
        type response = {
          posts : Types.Post.t list;
        }
        
        include Endpoint with type t := response
      
        val fetch :
            ?offset : int -> 
            ?filter : filter ->
            basename : string ->
            unit ->  
            (response_envelope, error) Core_kernel.Result.t Lwt.t
      
      end
      
      type response = {
        blog : Types.Blog.t;
        posts : Types.Post.t list;
        total_posts : int;
      }
      
      include Endpoint with type t := response
    
      val fetch :
          ?type' : Types.Post_type.t ->
          ?id : int ->
          ?tag : string -> 
          ?limit : int -> 
          ?offset : int ->
          ?reblog_info : bool ->
          ?notes_info : bool ->
          ?filter : filter ->
          basename : string ->
          unit ->  
          (response_envelope, error) Core_kernel.Result.t Lwt.t
    
    end
    
    module Post : sig
    
      module TextPost : sig
        
        val post : 
            ?title : string ->
            body : string ->
            basename : string ->
            ?id : int ->
            ?state : Types.State.t ->
            ?tags : string list ->
            ?tweet : Types.Auto_Tweet.t ->
            ?date : Types.Calendar.t ->
            ?format : Types.Post_format.t ->
            ?slug : string ->
            unit ->
            (unit, error) Core_kernel.Result.t Lwt.t
            
      end    
      
      module PhotoPost : sig
        
        type datasource =
          | Source of Uri.t
          | Data of Pervasives.in_channel list
        
        val post : 
            ?caption : string ->
            ?link : Uri.t ->
            photo : datasource ->
            basename : string ->
            ?id : int ->
            ?state : Types.State.t ->
            ?tags : string list ->
            ?tweet : Types.Auto_Tweet.t ->
            ?date : Types.Calendar.t ->
            ?format : Types.Post_format.t ->
            ?slug : string ->
            unit ->
            (unit, error) Core_kernel.Result.t Lwt.t
            
      end
      
      module QuotePost : sig
        
        val post : 
            ?source : string ->
            quote : string ->
            basename : string ->
            ?id : int ->
            ?state : Types.State.t ->
            ?tags : string list ->
            ?tweet : Types.Auto_Tweet.t ->
            ?date : Types.Calendar.t ->
            ?format : Types.Post_format.t ->
            ?slug : string ->
            unit ->
            (unit, error) Core_kernel.Result.t Lwt.t
            
      end
      
      module LinkPost : sig
        
        val post : 
            ?title : string ->
            ?description : string ->
            url : Uri.t ->
            basename : string ->
            ?id : int ->
            ?state : Types.State.t ->
            ?tags : string list ->
            ?tweet : Types.Auto_Tweet.t ->
            ?date : Types.Calendar.t ->
            ?format : Types.Post_format.t ->
            ?slug : string ->
            unit ->
            (unit, error) Core_kernel.Result.t Lwt.t
            
      end
      
      module ChatPost : sig
        
        val post : 
            ?title : string ->
            conversation : string ->
            basename : string ->
            ?id : int ->
            ?state : Types.State.t ->
            ?tags : string list ->
            ?tweet : Types.Auto_Tweet.t ->
            ?date : Types.Calendar.t ->
            ?format : Types.Post_format.t ->
            ?slug : string ->
            unit ->
            (unit, error) Core_kernel.Result.t Lwt.t
            
      end    
      
      module AudioPost : sig
        
        type datasource =
          | External_url of Uri.t
          | Data of Pervasives.in_channel
        
        val post : 
            ?caption : string ->
            audio : datasource ->
            basename : string ->
            ?id : int ->
            ?state : Types.State.t ->
            ?tags : string list ->
            ?tweet : Types.Auto_Tweet.t ->
            ?date : Types.Calendar.t ->
            ?format : Types.Post_format.t ->
            ?slug : string ->
            unit ->
            (unit, error) Core_kernel.Result.t Lwt.t
            
      end    
      
      module VideoPost : sig
        
        type datasource =
          | Embed of string
          | Data of Pervasives.in_channel
        
        val post : 
            ?caption : string ->
            video : datasource ->
            basename : string ->
            ?id : int ->
            ?state : Types.State.t ->
            ?tags : string list ->
            ?tweet : Types.Auto_Tweet.t ->
            ?date : Types.Calendar.t ->
            ?format : Types.Post_format.t ->
            ?slug : string ->
            unit ->
            (unit, error) Core_kernel.Result.t Lwt.t
            
      end    
      
      module Reblog : sig
        
        val reblog : 
            ?comment : string ->
            id : int ->
            reblog_key : int ->
            basename : string ->
            unit ->
            (unit, error) Core_kernel.Result.t Lwt.t
            
      end    
      
      module Delete : sig
        
        val delete : 
            id : int ->
            basename : string ->
            (unit, error) Core_kernel.Result.t Lwt.t
            
      end
      
    end
    
  end

  module User : sig

    module Info : sig
      
      type response = {
        user : Types.User.t;
      }
      
      include Endpoint with type t := response
      
      val fetch : unit -> (response_envelope, error) Core_kernel.Result.t Lwt.t
      
    end
    
    module Dashboard : sig
      
      type response = {
        posts : Types.Post.t list;
      }
      
      include Endpoint with type t := response
      
      val fetch : 
          ?limit : int ->
          ?offset : int ->
          ?type' : Types.Post_type.t ->
          ?since_id : int ->
          ?reblog_info : bool ->
          ?notes_info : bool ->
          unit ->
          (response_envelope, error) Core_kernel.Result.t Lwt.t
      
    end
    
    module Likes : sig
      
      type response = {
        liked_posts : Types.Post.t list;
        liked_count : int;
      }
      
      include Endpoint with type t := response
      
      val fetch : 
          ?limit : int -> 
          ?offset : int -> 
          unit -> 
          (response_envelope, error) Core_kernel.Result.t Lwt.t 
          
    end
    
    module Following : sig
      
      type response = {
        total_blogs : int;
        blogs : Types.Blog.t list;
      }
      
      include Endpoint with type t := response
      
      val fetch : 
          ?limit : int -> 
          ?offset : int -> 
          unit -> 
          (response_envelope, error) Core_kernel.Result.t Lwt.t 
          
    end
    
    module Follow : sig

      val follow : url : Uri.t -> (unit, error) Core_kernel.Result.t Lwt.t     
      
    end
    
    module Unfollow : sig

      val unfollow : url : Uri.t -> (unit, error) Core_kernel.Result.t Lwt.t     
      
    end
    
    module Like : sig
      
      val like : id : int -> reblog_key : string -> (unit, error) Core_kernel.Result.t Lwt.t
      
    end
    
    module Unlike : sig
      
      val unlike : id : int -> reblog_key : string -> (unit, error) Core_kernel.Result.t Lwt.t
      
    end
    
  end
  
  module Tagged : sig
    
    type response = Types.Post.t list
    
    include Endpoint with type t := response
    
    val fetch : 
        ?before : Types.Calendar.t -> 
        ?limit : int ->
        ?filter : filter -> 
        tag : string ->
        unit -> 
        (response_envelope, error) Core_kernel.Result.t Lwt.t
    
  end

end

open Lwt
open Meta_conv.Open
open Json_conv
open Tiny_json

module R = Core_kernel.Result

module Make 
    (Client : Sociaml_oauth_client_v1_0a.Client.S)
    (AT : sig val access_token : Client.access_token end) = struct
        
  module Client = Client
        
  exception Parameter_exc of string * string
  
  type error = 
    | Parameter_error of string * string
    | Conversion_error of Tiny_json.Json.t Meta_conv.Error.t
    | Client_error of Client.error

  type filter =
    | Text
    | Raw

  let base_uri = "http://api.tumblr.com/v2"

  let authorized_post
      ?uri_parameters:(uri_parameters: (string * string) list option)
      ?body_parameters:(body_parameters: (string * string) list option)
      ?expect
      uri_fragment =
    Client.do_post_request
        ?uri_parameters
        ?body_parameters
        ?expect
        ~uri:(base_uri ^ uri_fragment |> Uri.of_string)
        ~access_token:AT.access_token () >>= function
    | R.Error e -> return (R.Error (Client_error e))
    | R.Ok _ -> return (R.Ok ())

  module Make_Endpoint
      (T : 
        sig
          type r 
          val json_of_r : (r, Json.t) Meta_conv.Types.Encoder.t
          val r_of_json : (r, Json.t) Meta_conv.Types.Decoder.t
        end) = struct
    
    type meta = {
      status : int;
      msg : string;  
    } with conv(json)
      
    type response_envelope = {
      meta : meta;
      response : T.r;
    } with conv(json)
    
    let parse_response = function
      | R.Error e -> return (R.Error (Client_error e))
      | R.Ok response ->
        response |> Json.parse |> response_envelope_of_json |> function
          | `Ok t -> return (R.Ok t)
          | `Error e -> return (R.Error (Conversion_error e))
    
    let authorized_get 
        ?uri_parameters:(uri_parameters: (string * string) list option)
        ?expect
        uri_fragment =
      Client.do_get_request
          ?uri_parameters
          ?expect
          ~uri:(base_uri ^ uri_fragment |> Uri.of_string)
          ~access_token:AT.access_token () >>= parse_response
    
  end
  
  let filter_optional_params =
    let open Core_kernel.Std in
    List.fold ~init:[] ~f:(fun acc (k, v, f) -> match v with
      | Some v' -> List.append acc [(k, f v')]
      | None -> acc)

  module Blog = struct
    
    module Info = struct
      
      type response = {
        blog : Types.Blog.t;
      } with conv(json)
      
      include Make_Endpoint(struct type r = response with conv(json) end)
      
      let fetch ~basename = 
        Printf.sprintf "/blog/%s/info" basename |>
        authorized_get ~uri_parameters: [("api_key", AT.access_token.Client.consumer_key)]
      
    end
    
    module Avatar = struct
      
      type response = {
        avatar_url : Types.Uri.t;
      } with conv(json)
      
      type size =
        | Size_16
        | Size_24
        | Size_30
        | Size_40
        | Size_48
        | Size_64
        | Size_96
        | Size_128
        | Size_512

      let size_to_int = function
        | Size_16 -> 16
        | Size_24 -> 24
        | Size_30 -> 30
        | Size_40 -> 40
        | Size_48 -> 48
        | Size_64 -> 64
        | Size_96 -> 96
        | Size_128 -> 128
        | Size_512 -> 512
      
      include Make_Endpoint(struct type r = response with conv(json) end)
      
      let fetch ?size ~basename () =
        Printf.sprintf "/blog/%s/avatar%s" basename 
            (match size with 
              | Some s -> size_to_int s |> Printf.sprintf "/%i"
              | None -> "" ) |> authorized_get ~expect:`Moved_permanently
      
    end
    
    module Likes = struct
      
      type response = {
        liked_posts : Types.Post.t list;
        liked_count : int;
      } with conv(json)
      
      include Make_Endpoint(struct type r = response with conv(json) end)
    
      let fetch ?limit ?offset ~basename () =
        let open Core_kernel.Std in
        let params = filter_optional_params 
            [("limit", limit, Int.to_string); ("offset", offset, Int.to_string)] |> 
                List.append [("api_key", AT.access_token.Client.consumer_key)]            
        in
        Printf.sprintf "/blog/%s/likes" basename |> authorized_get ~uri_parameters:params
    
    end
    
    module Followers = struct
      
      type response = {
        total_users : int;
        users : Types.User.t list;
        name : string mc_option;
        following : bool mc_option;
        url : Types.Uri.t mc_option;
        updated : Types.Calendar.t mc_option;
      } with conv(json)
      
      include Make_Endpoint(struct type r = response with conv(json) end)
    
      let fetch ?limit ?offset ~basename () =
        let open Core_kernel.Std in
        let params = filter_optional_params 
            [("limit", limit, Int.to_string); ("offset", offset, Int.to_string)]     
        in
        Printf.sprintf "/blog/%s/followers" basename |> authorized_get ~uri_parameters:params
    
    end
    
    module Posts = struct
      
      module Queue = struct
      
        type response = {
          posts : Types.Post.t list;
        } with conv(json)
        
        include Make_Endpoint(struct type r = response with conv(json) end)
      
        let fetch ?limit ?offset ?filter:(filter:filter option) ~basename () =
          let open Core_kernel.Std in
          let open Types.Post_type in
          let params = filter_optional_params [
              ("limit", limit, Int.to_string);
              ("offset", offset, Int.to_string)] |> 
            List.append (filter_optional_params [
                ("filter", filter, function | Text -> "text" | Raw -> "raw")])            
          in
          Printf.sprintf "/blog/%s/posts/queue" basename |> authorized_get ~uri_parameters:params
      
      end
      
      module Draft = struct
      
        type response = {
          posts : Types.Post.t list;
        } with conv(json)
        
        include Make_Endpoint(struct type r = response with conv(json) end)
      
        let fetch ?before_id ?filter:(filter:filter option) ~basename () =
          let open Core_kernel.Std in
          let open Types.Post_type in
          let params = filter_optional_params [
              ("before_id", before_id, Int.to_string)] |> 
            List.append (filter_optional_params [
                ("filter", filter, function | Text -> "text" | Raw -> "raw")])            
          in
          Printf.sprintf "/blog/%s/posts/draft" basename |> authorized_get ~uri_parameters:params
      
      end
      
      module Submission = struct
      
        type response = {
          posts : Types.Post.t list;
        } with conv(json)
        
        include Make_Endpoint(struct type r = response with conv(json) end)
      
        let fetch ?offset ?filter:(filter:filter option) ~basename () =
          let open Core_kernel.Std in
          let open Types.Post_type in
          let params = filter_optional_params [
              ("offset", offset, Int.to_string)] |> 
            List.append (filter_optional_params [
                ("filter", filter, function | Text -> "text" | Raw -> "raw")])            
          in
          Printf.sprintf "/blog/%s/posts/submission" basename |> authorized_get ~uri_parameters:params
      
      end
      
      type response = {
        blog : Types.Blog.t;
        posts : Types.Post.t list;
        total_posts : int;
      } with conv(json)
      
      include Make_Endpoint(struct type r = response with conv(json) end)
    
      let fetch ?type':(type':Types.Post_type.t option) ?id ?tag ?limit ?offset 
          ?reblog_info ?notes_info ?filter:(filter:filter option) ~basename () =
        let open Core_kernel.Std in
        let open Types.Post_type in
        let params = filter_optional_params [
            ("id", id, Int.to_string);
            ("limit", limit, Int.to_string);
            ("offset", offset, Int.to_string)] |> 
          List.append (filter_optional_params [
              ("type", type', function 
                | Text -> "text"
                | Quote -> "quote"
                | Link -> "link"
                | Answer -> "answer"
                | Video -> "video"
                | Audio -> "audio"
                | Photo -> "photo"
                | Chat -> "chat")]) |>
          List.append (filter_optional_params [("tag", tag, fun s -> s)]) |>
          List.append (filter_optional_params [
              ("reblog_info", reblog_info, string_of_bool);
              ("notes_info", notes_info, string_of_bool)]) |>
          List.append (filter_optional_params [
              ("filter", filter, function | Text -> "text" | Raw -> "raw")]) |>
          List.append [("api_key", AT.access_token.Client.consumer_key)]            
        in
        Printf.sprintf "/blog/%s/posts" basename |> authorized_get ~uri_parameters:params
    
    end
    
    module Post = struct
      
      open CalendarLib
      open Core_kernel.Std
      open Types.Auto_Tweet
      open Types.Post_format
      open Types.State
      
      let rfc2616_format = "%a, %d %b %Y %H:%M:%S GMT"
      
      let post ~params ~type' ~basename ?id ?state ?tags ?tweet ?date ?format ?slug () =
        try 
          let params = List.append params ["type", type'] |>
            List.append (filter_optional_params [("state", state, function
              | Published -> "published"
              | Queued -> "queued"
              | Draft -> "draft"
              | Private -> "private");]) |>
            List.append (filter_optional_params [("tags", tags, fun l -> 
              let out = new Csv_util.buffer_out_channel in
              let out_obj = Csv.to_out_obj ~separator:',' (out :> Csv.out_obj_channel) in
              Csv.output_record out_obj l;
              out#contents () |> Core_kernel.Std.String.strip);]) |>
            List.append (filter_optional_params [("tweet", tweet, function
              | Off -> "off"
              | On s -> s);]) |>
            List.append (filter_optional_params [("date", date, fun d -> 
              Calendar.to_gmt d |> Printer.Calendar.sprint rfc2616_format);]) |>
            List.append (filter_optional_params [("format", format, function
              | Markdown -> "markdown"
              | HTML -> "html"
              | Raw -> raise (Parameter_exc("format", "Raw is not a valid posting format")));]) |>
            List.append (filter_optional_params [("slug", slug, fun s -> s);]) |>
            List.append (filter_optional_params [("id", id, Int.to_string);]) 
          in
          (match id with
          | Some _ -> 
            Printf.sprintf "/blog/%s/post/edit" basename |>
                authorized_post ~body_parameters:params
          | None -> 
            Printf.sprintf "/blog/%s/post" basename |> 
                authorized_post ~expect:`Created ~body_parameters:params) 
              
        with Parameter_exc (param, msg) ->
          return (R.Error(Parameter_error (param, msg)))
    
      module TextPost = struct
        
        let post ?title ~body = post 
            ~params:(List.append (filter_optional_params [("title", title, fun s -> s)]) ["body", body])
            ~type':"text"          
            
      end    
      
      module PhotoPost = struct
        
        type datasource =
          | Source of Uri.t
          | Data of in_channel list
        
        let post ?caption ?link ~photo = 
          let body = match photo with
          | Source uri -> ["source", Uri.to_string uri]
          | Data channels -> channels |> List.mapi ~f:(fun i channel ->
            let buf = Buffer.create 16 in
            let rec read () =
              let b = String.create 100 in
              match Pervasives.input channel b 0 100 with
              | 0 -> ()
              | count -> 
                Buffer.add_substring buf b 0 count;
                read ()
            in
            read ();
            (Printf.sprintf "data[%i]" i, Buffer.contents buf))  in
          post 
            ~params:(body |> 
                List.append (filter_optional_params [("caption", caption, fun s -> s)]) |> 
                List.append (filter_optional_params [("link", link, Uri.to_string)]))
            ~type':"photo"      
            
      end
      
      module QuotePost = struct
        
        let post ?source ~quote = post 
            ~params:(List.append (filter_optional_params [("source", source, fun s -> s)]) ["quote", quote])
            ~type':"quote"          
            
      end
      
      module LinkPost = struct
        
        let post ?title ?description ~url = post 
            ~params:(List.append (filter_optional_params [("title", title, fun s -> s);
                ("description", description, fun s -> s)]) ["url", Uri.to_string url])
            ~type':"link"          
            
      end
      
      module ChatPost = struct
        
        let post ?title ~conversation = post 
            ~params:(List.append (filter_optional_params [("title", title, fun s -> s)]) ["conversation", conversation])
            ~type':"chat"          
            
      end    
      
      module AudioPost = struct
        
        type datasource =
          | External_url of Uri.t
          | Data of Pervasives.in_channel
        
        let post ?caption ~audio = 
          let body = match audio with
          | External_url uri -> ["external_url", Uri.to_string uri]
          | Data channel ->
            let buf = Buffer.create 16 in
            let rec read () =
              let b = String.create 100 in
              match Pervasives.input channel b 0 100 with
              | 0 -> ()
              | count -> 
                Buffer.add_substring buf b 0 count;
                read ()
            in
            read ();
            [("data", Buffer.contents buf)]  
          in
          post 
            ~params:(body |> 
                List.append (filter_optional_params [("caption", caption, fun s -> s)]))
            ~type':"audio"      
            
      end    
      
      module VideoPost = struct
        
        type datasource =
          | Embed of string
          | Data of Pervasives.in_channel
        
        let post ?caption ~video = 
          let body = match video with
          | Embed embed -> ["embed", embed]
          | Data channel ->
            let buf = Buffer.create 16 in
            let rec read () =
              let b = String.create 100 in
              match Pervasives.input channel b 0 100 with
              | 0 -> ()
              | count -> 
                Buffer.add_substring buf b 0 count;
                read ()
            in
            read ();
            [("data", Buffer.contents buf)]  
          in
          post 
            ~params:(body |> 
                List.append (filter_optional_params [("caption", caption, fun s -> s)]))
            ~type':"video"      
            
      end
      
      module Reblog = struct
        
        let reblog ?comment ~id ~reblog_key ~basename () =
          let params = List.append [("id", Int.to_string id); 
              ("reblog_key", Int.to_string reblog_key)] 
              (filter_optional_params [("comment", comment, fun s -> s);]) 
          in
          Printf.sprintf "/blog/%s/post/reblog" basename |> authorized_post 
              ~expect:`Created 
              ~body_parameters:params
                           
      end
      
      module Delete = struct
        
        let delete ~id ~basename =
          Printf.sprintf "/blog/%s/post/delete" basename |> authorized_post  
              ~body_parameters:[("id", Int.to_string id)]
                           
      end
      
    end
    
  end
  
  module User = struct
    
    module Info = struct
      
      type response = {
        user : Types.User.t;
      } with conv(json)
      
      include Make_Endpoint(struct type r = response with conv(json) end)
      
      let fetch () = authorized_get "/user/info"
      
    end
    
    module Dashboard = struct
      
      type response = {
        posts : Types.Post.t list;
      } with conv(json)
      
      include Make_Endpoint(struct type r = response with conv(json) end)
      
      let fetch ?limit ?offset ?type':(type': Types.Post_type.t option) 
          ?since_id ?reblog_info ?notes_info () =
        let open Core_kernel.Std in
        let open Types.Post_type in
        let params = filter_optional_params [
            ("offset", offset, Int.to_string);
            ("limit", limit, Int.to_string);
            ("since_id", since_id, Int.to_string)] |> 
          List.append (filter_optional_params [
              ("type", type', function 
                | Text -> "text"
                | Quote -> "quote"
                | Link -> "link"
                | Answer -> "answer"
                | Video -> "video"
                | Audio -> "audio"
                | Photo -> "photo"
                | Chat -> "chat")]) |>
          List.append (filter_optional_params [
              ("reblog_info", reblog_info, string_of_bool);
              ("notes_info", notes_info, string_of_bool)])            
        in
        authorized_get ~uri_parameters:params "/user/dashboard"
      
    end
    
    module Likes = struct
      
      type response = {
        liked_posts : Types.Post.t list;
        liked_count : int;
      } with conv(json)
      
      include Make_Endpoint(struct type r = response with conv(json) end)
      
      let fetch ?limit ?offset () =
        let open Core_kernel.Std in
        let params = filter_optional_params 
            [("limit", limit, Int.to_string); ("offset", offset, Int.to_string)]
        in
        authorized_get ~uri_parameters:params "/user/likes"
      
    end
    
    module Following = struct
      
      type response = {
        total_blogs : int;
        blogs : Types.Blog.t list;
      } with conv(json)
      
      include Make_Endpoint(struct type r = response with conv(json) end)
      
      let fetch ?limit ?offset () =
        let open Core_kernel.Std in
        let params = filter_optional_params 
            [("limit", limit, Int.to_string); ("offset", offset, Int.to_string)]
        in
        authorized_get ~uri_parameters:params "/user/following"
      
    end
    
    module Follow = struct

      let follow ~url = authorized_post ~body_parameters:[("url", Uri.to_string url)] "/user/follow"     
      
    end
    
    module Unfollow = struct

      let unfollow ~url = authorized_post ~body_parameters:[("url", Uri.to_string url)] "/user/unfollow"     
      
    end
    
    module Like = struct
      
      open Core_kernel.Std
      
      let like ~id ~reblog_key =
        authorized_post ~body_parameters:[
          ("id", Int.to_string id);
          ("reblog_key", reblog_key)] "/user/like"
      
    end
    
    module Unlike = struct
      
      open Core_kernel.Std
      
      let unlike ~id ~reblog_key =
        authorized_post ~body_parameters:[
          ("id", Int.to_string id);
          ("reblog_key", reblog_key)] "/user/unlike"
      
    end
    
  end
  
  module Tagged = struct
    
    type response = Types.Post.t list with conv(json)
    
    include Make_Endpoint(struct type r = response with conv(json) end)
    
    let fetch ?before ?limit ?filter ~tag () =
        let open Core_kernel.Std in
        let params = filter_optional_params [
            ("before", before, (fun c ->
                CalendarLib.Calendar.to_unixfloat c |> Float.to_int |> Int.to_string))] |>
            List.append (filter_optional_params [("limit", limit, Int.to_string)]) |>
            List.append (filter_optional_params [
                ("filter", filter, function | Text -> "text" | Raw -> "raw")]) |>
            List.append [("tag", tag); ("api_key", AT.access_token.Client.consumer_key)]
        in
        authorized_get ~uri_parameters:params "/tagged"
    
  end
  
end