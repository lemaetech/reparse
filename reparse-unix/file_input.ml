external unsafe_pread
  :  Unix.file_descr
  -> bytes
  -> int
  -> int
  -> int
  -> int
  = "caml_pread"

let create fd =
  let rec really_read fd buf len fd_offset buf_offset =
    if len <= 0
    then ()
    else (
      match unsafe_pread fd buf len fd_offset buf_offset with
      | 0 -> raise End_of_file
      | r -> really_read fd buf (len - r) (fd_offset + r) (buf_offset + r))
  in
  object (self)
    method nth offset =
      let buf = Bytes.create 1 in
      match unsafe_pread fd buf 1 offset 0 with
      | 0 -> raise End_of_file
      | 1 -> Bytes.get buf 0
      | _ -> failwith "IO.Unix_fd.nth"

    method eof offset =
      match self#nth offset with
      | (_ : char) -> false
      | exception _ -> true

    method sub ~offset ~len =
      let buf = Bytes.create len in
      really_read fd buf len offset 0;
      Bytes.to_string buf
  end
;;
