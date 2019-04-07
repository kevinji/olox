open Core
open Async
open! Import

let run ~source =
  match Scanner.parse ~source with
  | Ok tokens -> List.iter tokens ~f:(printf !"%{Token}\n")
  | Error error -> printf !"%{Error#hum}\n" error
;;

let run_file ~file =
  let open Deferred.Or_error.Let_syntax in
  let%map source = Monitor.try_with_or_error (fun () -> Reader.file_contents file) in
  run ~source
;;

let repl () =
  let open Deferred.Or_error.Let_syntax in
  let stdin = force Reader.stdin in
  let rec read_and_run () =
    print_string "> ";
    match%bind Reader.read_line stdin |> Deferred.ok with
    | `Ok source ->
      run ~source;
      read_and_run ()
    | `Eof ->
      print_newline ();
      return ()
  in
  read_and_run ()
;;
