class t : Unix.file_descr -> Reparse.Parser.input
(** Represents a unix file descriptor as a parser input.

    {4:file_input_examples Examples}

    {[
      module P = Reparse.Parser
      ;;
      let fd = Unix.openfile fname [Unix.O_RDWR; Unix.O_CREAT] 0o640 in
      let file_input = new Reparse_unix.File_input.t fd
    ]} *)
