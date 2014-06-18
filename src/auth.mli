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

module Make (Client : Oauth_client_v1_0a.Client.S) : S