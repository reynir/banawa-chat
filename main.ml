module RNG = Mirage_crypto_rng.Fortuna

let _2s = 2_000_000_000
let ( let@ ) finally fn = Fun.protect ~finally fn
let rng () = Mirage_crypto_rng_mkernel.initialize (module RNG)
let rng = Mkernel.map rng Mkernel.[]

type client = {
    env: (string, string) Hashtbl.t
  ; sigwinch: Mnotty.Signal.t
  ; mutable size: int * int
}

let replace buf chr0 chr1 =
  for idx = 0 to Bytes.length buf - 1 do
    if Bytes.unsafe_get buf idx = chr0 then Bytes.unsafe_set buf idx chr1
  done

let room = Rb.make 1024
let vroom = Lwd.var room

let handler _flow client username = function
  | Mnet_ssh.Pty_req { width; height; _ } ->
      client.size <- (Int32.to_int width, Int32.to_int height)
  | Pty_set { width; height; _ } ->
      Mnotty.Signal.signal client.sigwinch
        (Int32.to_int width, Int32.to_int height)
  | Set_env _ -> ()
  | Shell { ic; oc; ec= _ } ->
      let ic () =
        match ic () with
        | Some str ->
            let buf = Bytes.of_string str in
            replace buf '\r' '\n';
            Some (Bytes.unsafe_to_string buf)
        | None -> None
      in
      let cursor = Lwd.var (0, 0) in
      let stop = Mnotty.Stop.create () in
      let message msg =
        let msg = Message.make ~nickname:username msg in
        Lwd.set vroom (Rb.push room msg; room)
      in
      let quit () =
        Rb.push room (Message.msgf "%s left the chat" username);
        Lwd.set vroom room;
        Mnotty.Stop.switch stop
      in
      Rb.push room (Message.msgf "Welcome, %s!" username);
      Lwd.set vroom room;
      let ui =
        let prompt = Prompt.make ~quit ~message cursor in
        let window = Window.make vroom in
        Lwd.map2
          ~f:(fun window prompt -> Nottui.Ui.vcat [ window; prompt ])
          window prompt
      in
      Mnottui.run ~stop ~cursor (client.size, client.sigwinch) ui ic oc
  | Channel _ -> assert false

let devices ?gateway ~ipv6 cidr =
  let open Mkernel in
  [ rng; Mnet.stack ~name:"service" ?gateway ~ipv6 cidr ]

let rec clean_up orphans =
  match Miou.care orphans with
  | None | Some None -> ()
  | Some (Some prm) ->
      begin match Miou.await prm with
      | Ok () -> clean_up orphans
      | Error exn ->
          Logs.err (fun m ->
              m "Unexpected exception from a SSH client: %s"
                (Printexc.to_string exn));
          clean_up orphans
      end

module User = struct
  type t = Awa.Hostkey.pub

  let weight _ = 1
end

module Lru = Lru.M.Make (String) (User)

module Users = struct
  type t = Lru.t

  let verify lru user auth =
    match (Lru.find user lru, auth) with
    | None, Awa.Server.Pubkey pkauth ->
        if Awa.Server.verify_pubkeyauth ~user pkauth then begin
          Lru.add user pkauth.pubkey lru;
          Lru.trim lru;
          true
        end
        else false
    | _, Awa.Server.Password _ -> false
    | Some pubkey, Awa.Server.Pubkey pkauth ->
        let result =
          Awa.Server.verify_pubkeyauth ~user pkauth
          && Awa.Hostkey.pub_eq pubkey pkauth.pubkey
        in
        if result then Lru.promote user lru;
        result
end

let run _ (cidr, gateway, ipv6) priv =
  Mkernel.run (devices ?gateway ~ipv6 cidr) @@ fun rng (daemon, tcp, _udp) () ->
  let@ () = fun () -> Mirage_crypto_rng_mkernel.kill rng in
  let@ () = fun () -> Mnet.kill daemon in
  let db = Lru.create ~random:true 0x7ff in
  let db = Mnet_ssh.Database (db, (module Users)) in
  let rec go listen orphans =
    clean_up orphans;
    let flow = Mnet.TCP.accept tcp listen in
    let _ =
      Miou.async ~orphans @@ fun () ->
      let client =
        {
          env= Hashtbl.create 0x1
        ; sigwinch= Mnotty.Signal.create ()
        ; size= (0, 0)
        }
      in
      let handler = handler flow client in
      let@ () = fun () -> try Mnet.TCP.close flow with _ -> () in
      (* TODO(dinosaure): we can probably use [Miou.Ownership]. *)
      ignore (Mnet_ssh.server db priv flow handler)
    in
    go listen orphans
  in
  go (Mnet.TCP.listen tcp 22) (Miou.orphans ())

open Cmdliner

let output_options = "OUTPUT OPTIONS"
let verbosity = Logs_cli.level ~docs:output_options ()
let renderer = Fmt_cli.style_renderer ~docs:output_options ()

let utf_8 =
  let doc = "Allow binaries to emit UTF-8 characters." in
  Arg.(value & opt bool true & info [ "with-utf-8" ] ~doc)

let t0 = Mkernel.clock_monotonic ()
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let neg fn = fun x -> not (fn x)

let reporter sources ppf =
  let re = Option.map Re.compile sources in
  let print src =
    let some re = (neg List.is_empty) (Re.matches re (Logs.Src.name src)) in
    Option.fold ~none:true ~some re
  in
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let pp header _tags k ppf fmt =
      let t1 = Mkernel.clock_monotonic () in
      let delta = Float.of_int (t1 - t0) in
      let delta = delta /. 1_000_000_000. in
      Fmt.kpf k ppf
        ("[+%a][%a]%a[%a]: " ^^ fmt ^^ "\n%!")
        Fmt.(styled `Blue (fmt "%04.04f"))
        delta
        Fmt.(styled `Cyan int)
        (Stdlib.Domain.self () :> int)
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src)
    in
    match (level, print src) with
    | Logs.Debug, false -> k ()
    | _, true | _ -> msgf @@ fun ?header ?tags fmt -> pp header tags k ppf fmt
  in
  { Logs.report }

let regexp =
  let parser str =
    match Re.Pcre.re str with
    | re -> Ok (str, `Re re)
    | exception _ -> error_msgf "Invalid PCRegexp: %S" str
  in
  let pp ppf (str, _) = Fmt.string ppf str in
  Arg.conv (parser, pp)

let sources =
  let doc = "A regexp (PCRE syntax) to identify which log we print." in
  let open Arg in
  value & opt_all regexp [ ("", `None) ] & info [ "l" ] ~doc ~docv:"REGEXP"

let setup_sources = function
  | [ (_, `None) ] -> None
  | res ->
      let res = List.map snd res in
      let fn acc = function `Re re -> re :: acc | _ -> acc in
      let res = List.fold_left fn [] res in
      Some (Re.alt res)

let setup_sources = Term.(const setup_sources $ sources)

let setup_logs utf_8 style_renderer sources level =
  Option.iter (Fmt.set_style_renderer Fmt.stdout) style_renderer;
  Fmt.set_utf_8 Fmt.stdout utf_8;
  Logs.set_level level;
  Logs.set_reporter (reporter sources Fmt.stdout);
  Option.is_none level

let setup_logs =
  let open Term in
  const setup_logs $ utf_8 $ renderer $ setup_sources $ verbosity

let priv =
  let doc = "The private key of the unikernel" in
  let parser = Awa.Keys.of_string in
  let pp ppf _ = Fmt.pf ppf "#priv" in
  let priv = Arg.conv (parser, pp) in
  let open Arg in
  required & opt (some priv) None & info [ "priv" ] ~doc ~docv:"type:<base64>"

let term =
  let open Term in
  const run $ setup_logs $ Mnet_cli.setup $ priv

let cmd =
  let info = Cmd.info "banawa" in
  Cmd.v info term

let () = Cmd.(exit @@ eval cmd)
