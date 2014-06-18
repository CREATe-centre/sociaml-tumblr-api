class buffer_out_channel = object
  
  val buffer = Buffer.create 16
  
  method output data (offset : int) (length : int) =
    match String.length data with
    | l when l <= offset -> 0
    | l when l <= offset + length ->
      let count = l - offset in
      Buffer.add_substring buffer data offset count;
      count
    | _ -> 
      Buffer.add_substring buffer data offset length;
      length
  
  method close_out () = ()
  
  method contents () =
    Buffer.contents buffer
  
end