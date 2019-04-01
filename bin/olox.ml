open! Core
open Async
open Import

let command =
  Command.async_or_error
    ~summary:"olox interpreter"
    (let open Command.Let_syntax in
    let%map_open file =
      flag
        "-script"
        (optional file)
        ~doc:"FILE path to script to run; if not passed in, run in interactive mode"
    in
    fun () ->
      match file with
      | Some file -> Interpreter.run_file ~file
      | None -> Interpreter.repl ())
;;

let () = Command.run command
