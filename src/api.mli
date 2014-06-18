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
  
  module Client : Oauth_client_v1_0a.Client.S
  
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
          (response_envelope, error) Core.Result.t Lwt.t
    
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
          (response_envelope, error) Core.Result.t Lwt.t
      
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
          (response_envelope, error) Core.Result.t Lwt.t
    
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
          (response_envelope, error) Core.Result.t Lwt.t
    
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
            (response_envelope, error) Core.Result.t Lwt.t
      
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
            (response_envelope, error) Core.Result.t Lwt.t
      
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
            (response_envelope, error) Core.Result.t Lwt.t
      
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
          (response_envelope, error) Core.Result.t Lwt.t
    
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
            (unit, error) Core.Result.t Lwt.t
            
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
            (unit, error) Core.Result.t Lwt.t
            
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
            (unit, error) Core.Result.t Lwt.t
            
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
            (unit, error) Core.Result.t Lwt.t
            
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
            (unit, error) Core.Result.t Lwt.t
            
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
            (unit, error) Core.Result.t Lwt.t
            
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
            (unit, error) Core.Result.t Lwt.t
            
      end    
      
      module Reblog : sig
        
        val reblog : 
            ?comment : string ->
            id : int ->
            reblog_key : int ->
            basename : string ->
            unit ->
            (unit, error) Core.Result.t Lwt.t
            
      end    
      
      module Delete : sig
        
        val delete : 
            id : int ->
            basename : string ->
            (unit, error) Core.Result.t Lwt.t
            
      end
      
    end
    
  end

  module User : sig

    module Info : sig
      
      type response = {
        user : Types.User.t;
      }
      
      include Endpoint with type t := response
      
      val fetch : unit -> (response_envelope, error) Core.Result.t Lwt.t
      
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
          (response_envelope, error) Core.Result.t Lwt.t
      
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
          (response_envelope, error) Core.Result.t Lwt.t 
          
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
          (response_envelope, error) Core.Result.t Lwt.t 
          
    end
    
    module Follow : sig

      val follow : url : Uri.t -> (unit, error) Core.Result.t Lwt.t     
      
    end
    
    module Unfollow : sig

      val unfollow : url : Uri.t -> (unit, error) Core.Result.t Lwt.t     
      
    end
    
    module Like : sig
      
      val like : id : int -> reblog_key : string -> (unit, error) Core.Result.t Lwt.t
      
    end
    
    module Unlike : sig
      
      val unlike : id : int -> reblog_key : string -> (unit, error) Core.Result.t Lwt.t
      
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
        (response_envelope, error) Core.Result.t Lwt.t
    
  end

end

module Make 
    (Client : Oauth_client_v1_0a.Client.S)
    (AT : sig val access_token : Client.access_token end) : S