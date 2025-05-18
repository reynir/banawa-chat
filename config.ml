(* mirage >= 4.9.0 *)
open Mirage

let main =
  let runtime_args = [
    runtime_arg ~pos:__POS__ "Unikernel.K.port";
    runtime_arg ~pos:__POS__ "Unikernel.K.hostkey";
  ] in
  let packages = [
    package "banawa-mirage" ~pin:"git+https://github.com/sorbusursina/banawa-ssh.git#banawa";
    package "awa" ~pin:"git+https://github.com/mirage/awa-ssh.git#authie";
    package "notty";
    package "nottui"
      ~pin:"git+https://github.com/reynir/lwd.git#split-out-unix";
    package "lwd"
      ~pin:"git+https://github.com/reynir/lwd.git#split-out-unix";
    package "art";
  ] in
  main ~runtime_args ~packages "Unikernel.Main" (stackv4v6 @-> job)

let () =
  register "banawa-chat" [
    main $ generic_stackv4v6 default_network
  ]
