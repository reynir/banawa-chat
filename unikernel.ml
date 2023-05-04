open Lwt.Syntax

type state =
  { env : (string, string) Hashtbl.t
  ; sigwinch : (int * int) Lwt_condition.t
  ; mutable size : int * int
  }

module Main (_ : Mirage_random.S) (T : Mirage_time.S) (M : Mirage_clock.MCLOCK) (Stack : Tcpip.Stack.V4V6) = struct
  module Ssh = Banawa_mirage.Make(Stack.TCP)(T)(M)
  module Nottui' = Nottui_mirage.Make(T)

  let buffer = Rb.make 1024
  let buffer_var = Lwd.var buffer

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
    | Ssh.Channel { cmd; ic=_; oc=_; ec } ->
      let* () =
        ec (Printf.ksprintf Cstruct.of_string
              "Thanks for logging in! Currently, %S is unsupported\r\n\
               Check back later." cmd)
      in
      let* () = Lwt_switch.turn_off stop in
      Stack.TCP.close flow
    | Ssh.Shell { ic; oc; ec=_ } ->
      let ic () =
        let+ r = ic () in
        match r with
        | `Data cs -> `Data (Cstruct.map (function '\r' -> '\n' | c -> c) cs)
        | `Eof -> `Eof
      in
      let cursor = Lwd.var (0, 0) in
      let message m =
        let msg = Message.make ~nickname:username m in
        Lwd.set buffer_var (Rb.push buffer msg; buffer);
      in
      Rb.push buffer (Message.msgf "Welcome, %s!" username);
      let ui =
        let ( let* ) x f = Lwd.bind x ~f in
        let* prompt = Prompt.make ~message cursor in
        let* window = Window.make buffer_var in
        Lwd.return (Nottui.Ui.vcat [window; prompt])
      in
      Lwt.join [
        Nottui'.run ~cursor (t.size, t.sigwinch) ui ic oc;
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
