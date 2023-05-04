open Lwt.Syntax

type state =
  { env : (string, string) Hashtbl.t
  ; sigwinch : (int * int) Lwt_condition.t
  ; mutable size : int * int
  }

module Main (_ : Mirage_random.S) (T : Mirage_time.S) (M : Mirage_clock.MCLOCK) (Stack : Tcpip.Stack.V4V6) = struct
  module Ssh = Banawa_mirage.Make(Stack.TCP)(T)(M)
  module Nottui' = Nottui_mirage.Make(T)

  let c : Message.t Lwt_condition.t = Lwt_condition.create ()

  let read username ic =
    let rec loop () =
      let* r = ic () in
      match r with
      | `Data d ->
        let message = Cstruct.to_string d in
        if String.equal message "" then
          loop ()
        else
          let m = Message.make ~nickname:username (String.trim message) in
          Lwt_condition.broadcast c m;
          loop ()
      | `Eof ->
        Lwt_condition.broadcast c (Message.make ~nickname:"<--" username);
        Lwt.return_unit
    in
    loop ()

  let rec write me oc =
    let* m = Lwt_condition.wait c in
    if String.equal (Message.nickname m) me then
      write me oc
    else
      let* () =
        oc (Printf.ksprintf Cstruct.of_string "\x07%s: %s\r\n"
              (Message.nickname m) (Message.message m))
      in
      write me oc

  let chat flow stop username ic oc =
    Lwt_condition.broadcast c (Message.make ~nickname:"-->" username);
    let* () = oc (Cstruct.of_string (Printf.sprintf "Hello, %s!\r\n" username)) in
    let* () =
      Lwt.pick [
        read username ic;
        write username oc;
      ]
    in
    let* () = Lwt_switch.turn_off stop in
    Stack.TCP.close flow

  let callback flow stop t ~username r =
    match r with
    | Ssh.Pty_req { width; height; _ } ->
      t.size <- (Int32.to_int width, Int32.to_int height);
      Lwt.return_unit
    | Ssh.Pty_set { width; height; _ } ->
      Lwt_condition.broadcast t.sigwinch
        (Int32.to_int width, Int32.to_int height);
      Lwt.return_unit
    | Ssh.Set_env _ -> Lwt.return_unit
    | Ssh.Channel { cmd=_; ic; oc; ec=_ } ->
      chat flow stop username ic oc
    | Ssh.Shell { ic; oc; ec=_ } ->
      let ic () =
        let+ r = ic () in
        match r with
        | `Data cs -> `Data (Cstruct.map (function '\r' -> '\n' | c -> c) cs)
        | `Eof -> `Eof
      in
      let cursor = Lwd.var (0, 0) in
      let message m = Lwt_condition.broadcast c (Message.make ~nickname:username m) in
      let buffer = Rb.make 100 in
      Rb.push buffer (Message.msgf "Welcome, %s!" username);
      let buffer_var = Lwd.var buffer in
      let ui =
        let ( let* ) x f = Lwd.bind x ~f in
        let* prompt = Prompt.make ~message cursor in
        let* window = Window.make buffer_var in
        Lwd.return (Nottui.Ui.vcat [window; prompt])
      in
      let rec handle_receive () =
        let* msg = Lwt_condition.wait c in
        Lwd.set buffer_var (Rb.push buffer msg; buffer);
        handle_receive ()
      in
      Lwt_condition.broadcast c (Message.make ~nickname:"-->" username);
      Lwt.join [
        Nottui'.run ~cursor (t.size, t.sigwinch) ui ic oc;
        handle_receive ();
      ]

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
         let state =
           { env = Hashtbl.create 0x10
           ; sigwinch = Lwt_condition.create ()
           ; size = (0, 0)
           }
         in
         let _ssh = Ssh.spawn_server ~stop server msgs flow (callback flow stop state) in
         Lwt.return_unit);
    fst (Lwt.wait ())
end
