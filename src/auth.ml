module type S = sig
  module Client : Oauth_client_v1_0a.Client.S
  val fetch_request_token : 
      consumer_key : string -> 
      consumer_secret : string ->
      (Client.request_token, Client.error) Core.Result.t Lwt.t
  val fetch_access_token : 
      request_token : Client.request_token ->
      verifier : string -> 
      (Client.access_token, Client.error) Core.Result.t Lwt.t
end

module Make (Client : Oauth_client_v1_0a.Client.S) = struct
  
  module Client = Client
  
  let fetch_request_token =
    let request_uri = Uri.of_string "http://www.tumblr.com/oauth/request_token" in
    let authorization_uri = Uri.of_string "http://www.tumblr.com/oauth/authorize" in
    fun ~consumer_key ~consumer_secret ->  
      Client.fetch_request_token
          ~request_uri: request_uri
          ~authorization_uri: authorization_uri
          ~consumer_key: consumer_key
          ~consumer_secret: consumer_secret
          ()
        
  let fetch_access_token =
    let access_uri = Uri.of_string "http://www.tumblr.com/oauth/access_token" in
    fun ~request_token ~verifier ->
      Client.fetch_access_token
          ~access_uri: access_uri
          ~request_token: request_token
          ~verifier: verifier 
            
end