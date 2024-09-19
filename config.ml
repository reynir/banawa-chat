(* mirage >= 4.7.0 *)
open Mirage

let main =
  let runtime_args = [
    runtime_arg ~pos:__POS__ "Unikernel.K.port";
    runtime_arg ~pos:__POS__ "Unikernel.K.hostkey";
  ] in
  let packages = [
    package "awa" ~pin:"git+https://github.com/reynir/awa-ssh.git#banawa";
    package "banawa-mirage" ~pin:"git+https://github.com/sorbusursina/banawa-ssh.git#awa";
    package "notty";
    package "nottui"
      ~pin:"git+https://github.com/dinosaure/lwd.git#9e78758d5987597bac65fe73bd30ff80741cfe83";
    package "lwd"
      ~pin:"git+https://github.com/dinosaure/lwd.git#9e78758d5987597bac65fe73bd30ff80741cfe83";
    package "art";
  ] in
  main ~runtime_args ~packages "Unikernel.Main" (random @-> time @-> mclock @-> stackv4v6 @-> job)

let () =
  register "banawa-chat" [
    main $ default_random $ default_time $ default_monotonic_clock $ generic_stackv4v6 default_network
  ]
