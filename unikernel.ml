open Lwt.Syntax

type state =
  { env : (string, string) Hashtbl.t
  ; sigwinch : (int * int) Lwt_condition.t
  ; mutable size : int * int
  }

module K = struct
  open Cmdliner
  let port =
    let doc = Arg.info ~doc:"The TCP port for listening for SSH connections" ["port"] in
    Arg.(value & opt int 22 doc)

  let hostkey =
    let doc = Arg.info ~doc:"SSH host key" ["hostkey"] in
    Arg.(required & opt (some string) None doc)
end

module Main (Stack : Tcpip.Stack.V4V6) = struct
  module Ssh = Banawa_mirage.Make(Stack.TCP)
  module Nottui' = Nottui_mirage.Make

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
      let quit () =
        let msg = Message.msgf "%s tried to quit, but it is not implemented" username in
        Lwd.set buffer_var (Rb.push buffer msg; buffer);
      in
      Lwd.set buffer_var
        (Rb.push buffer (Message.msgf "Welcome, %s!" username); buffer);
      let ui =
        let ( let* ) x f = Lwd.bind x ~f in
        let* prompt = Prompt.make ~quit ~message cursor in
        let* window = Window.make buffer_var in
        Lwd.return (Nottui.Ui.vcat [window; prompt])
      in
      Lwt.join [
        Nottui'.run ~cursor (t.size, t.sigwinch) ui ic oc;
      ]

  let start stack port hostkey =
    let hostkey =
      match Awa.Keys.of_string hostkey with
      | Ok k -> k
      | Error `Msg e ->
        Logs.err (fun m -> m "%s" e); exit 1
    in
    let server, msgs = Awa.Server.make hostkey in
    Stack.TCP.listen (Stack.tcp stack) ~port
      (fun flow ->
         let stop = Lwt_switch.create () in
         let state =
           { env = Hashtbl.create 0x10
           ; sigwinch = Lwt_condition.create ()
           ; size = (0, 0)
           }
         in
         let db = Banawa_mirage.Auth.empty 42 in
         let _ssh = Ssh.spawn_server ~stop server db msgs flow (callback flow stop state) in
         Lwt.return_unit);
    fst (Lwt.wait ())
end
