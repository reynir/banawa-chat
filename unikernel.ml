open Lwt.Syntax

module Main (_ : Mirage_random.S) (T : Mirage_time.S) (M : Mirage_clock.MCLOCK) (Stack : Tcpip.Stack.V4V6) = struct
  module Ssh = Banawa_mirage.Make(Stack.TCP)(T)(M)

  type message =
    | Message of { sender : string; message : string }
    | Join of string
    | Part of string

  let c : message Lwt_condition.t = Lwt_condition.create ()

  let read username ic =
    let rec loop () =
      let* r = ic () in
      match r with
      | `Data d ->
        let message = Cstruct.to_string d in
        if String.equal message "" then
          loop ()
        else
          let m = Message {
              sender = username;
              message = String.trim message;
            } in
          Lwt_condition.broadcast c m;
          loop ()
      | `Eof ->
        Lwt_condition.broadcast c (Part username);
        Lwt.return_unit
    in
    loop ()

  let rec write me oc =
    let* m = Lwt_condition.wait c in
    match m with
    | Message { sender; message=_ } when String.equal sender me ->
      write me oc
    | Message { sender; message } ->
      let* () = oc (Printf.ksprintf Cstruct.of_string "%s: %s\r\n" sender message) in
      write me oc
    | Join username ->
      let* () = oc (Printf.ksprintf Cstruct.of_string "--> %s joined!\r\n" username) in
      write me oc
    | Part username ->
      let* () = oc (Printf.ksprintf Cstruct.of_string "<-- %s left\r\n" username) in
      write me oc


  let chat flow stop username ic oc =
    Lwt_condition.broadcast c (Join username);
    let* () =
      Lwt.pick [
        read username ic;
        write username oc;
      ]
    in
    let* () = Lwt_switch.turn_off stop in
    Stack.TCP.close flow

  let callback flow stop ~username r =
    match r with
    | Ssh.Pty_req _ | Ssh.Pty_set _ | Ssh.Set_env _ ->
      Lwt.return_unit
    | Ssh.Channel { cmd; ic; oc; ec; } ->
      let* () = oc (Cstruct.of_string (Printf.sprintf "Hello, %s!\r\n" username)) in
      chat flow stop username ic oc
    | Ssh.Shell { ic; oc; ec; } ->
      let* () = oc (Cstruct.of_string (Printf.sprintf "Hello, %s!\r\n" username)) in
      chat flow stop username ic oc

  let start _random _time _mtime stack =
    let port = Key_gen.port () in
    let user_db = Banawa.Auth.Db.create 20 in
    let hostkey = Key_gen.hostkey () in
    let hostkey =
      match Banawa.Keys.of_string hostkey with
      | Ok k -> k
      | Error `Msg e ->
        Logs.err (fun m -> m "%s" e); exit 1
    in
    let server, msgs = Banawa.Server.make hostkey user_db in
    Stack.TCP.listen (Stack.tcp stack) ~port
      (fun flow ->
         let stop = Lwt_switch.create () in
         let _ssh = Ssh.spawn_server ~stop server msgs flow (callback flow stop) in
         Lwt.return_unit);
    fst (Lwt.wait ())
end
