open! Core
open! Async
open! Import

let command =
  Command.async_or_error
    ~summary:"olox interpreter"
    (let%map_open.Command file =
       flag
         "-script"
         (optional Filename_unix.arg_type)
         ~doc:"FILE path to script to run; if not passed in, run in interactive mode"
     in
     fun () ->
       match file with
       | Some file -> Interpreter.run_file ~file
       | None -> Interpreter.repl ())
;;

let () = Command_unix.run command
