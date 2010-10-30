(* Copyright (c) 2010 Mauricio Fernández <mfp@acm.org> *)
open Printf
open Lwt

let set_some_string r = Arg.String (fun s -> r := Some s)
let set_some_int r = Arg.Int (fun n -> r := Some n)


let port = ref 61613
let debug = ref false
let login = ref None
let passcode = ref None

let params =
  Arg.align
    [
      "-port", Arg.Set_int port, "PORT Port to listen at (default: 61613).";
      "-login", set_some_string login, "LOGIN Login expected in CONNECT.";
      "-passcode", set_some_string passcode, "PASSCODE Passcode expected in CONNECT.";
      "-debug", Arg.Set debug, " Write debug info to stderr.";
    ]

let usage_message = "Usage: ocamlmq [options]"

let _ = Sys.set_signal Sys.sigpipe Sys.Signal_ignore
let _ = Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> exit 0))

module SERVER = Mq_server.Make(Mq_hashtable_persistence)

let () =
  Arg.parse
    params
    (fun s -> eprintf "Unknown argument: %S\n%!" s;
              Arg.usage params usage_message;
              exit 1)
    usage_message;
  let addr = Unix.ADDR_INET (Unix.inet_addr_any, !port) in
    Lwt_unix.run begin
      let msg_store = Mq_hashtable_persistence.create !debug
      in
        if !debug then eprintf "Connected to database.\n%!";
        (if !initdb then begin
           eprintf "Initializing database.\n%!";
           Mq_hashtable_persistence.initialize msg_store
         end else return ()) >>
        lwt broker = SERVER.make_broker
                       ?login:!login ?passcode:!passcode msg_store addr
        in SERVER.server_loop ~debug:!debug broker
    end
