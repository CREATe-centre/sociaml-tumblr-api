class buffer_out_channel : object
  inherit Csv.out_obj_channel
  method contents : unit -> string
end