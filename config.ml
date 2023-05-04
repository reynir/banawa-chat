open Mirage

let port =
  let doc = Key.Arg.info ~doc:"The TCP port for listening for SSH connections" ["port"] in
  Key.(create "port" Arg.(opt int 22 doc))

let hostkey =
  let doc = Key.Arg.info ~doc:"SSH host key" ["hostkey"] in
  Key.(create "hostkey" Arg.(required string doc))

let main =
  let packages = [
    package "banawa" ~pin:"git+https://github.com/sorbusursina/banawa-ssh.git";
    package "banawa-mirage" ~pin:"git+https://github.com/sorbusursina/banawa-ssh.git";
    package "notty";
    package "nottui"
      ~pin:"git+https://github.com/dinosaure/lwd.git#9e78758d5987597bac65fe73bd30ff80741cfe83";
    package "lwd"
      ~pin:"git+https://github.com/dinosaure/lwd.git#9e78758d5987597bac65fe73bd30ff80741cfe83";
    package "art";
  ] in
  let keys = [ Key.v port ; Key.v hostkey ] in
  foreign ~keys ~packages "Unikernel.Main" (random @-> time @-> mclock @-> stackv4v6 @-> job)

let () =
  register "banawa-chat" [
    main $ default_random $ default_time $ default_monotonic_clock $ generic_stackv4v6 default_network
  ]
