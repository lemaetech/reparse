val create : Unix.file_descr -> Reparse.Parser.input
(** [create fd] creates a {!Reparse.Parser.input} instance from [fd].

    {[
      let fd = Unix.openfile fname [Unix.O_RDWR; Unix.O_CREAT] 0o640 in
      let file_input = Reparse_unix.File_input.create fd
    ]} *)
