open Core
open Async

let _fail ~line ~message = Or_error.errorf "[line %d] Error: %s\n" line message

let run ~source =
  let open Deferred.Or_error.Let_syntax in
  String.iter source ~f:(fun token -> printf "%c\n" token);
  return ()
;;

let run_file ~file =
  let open Deferred.Or_error.Let_syntax in
  let%bind source = Monitor.try_with_or_error (fun () -> Reader.file_contents file) in
  run ~source
;;

let repl () =
  let open Deferred.Or_error.Let_syntax in
  let stdin = force Reader.stdin in
  let rec read_and_run () =
    print_string "> ";
    match%bind Reader.read_line stdin |> Deferred.ok with
    | `Ok source ->
      let%bind () = run ~source in
      read_and_run ()
    | `Eof ->
      print_newline ();
      return ()
  in
  read_and_run ()
;;
