type t = { nickname: string; message: string }

let nickname { nickname; _ } = nickname
let message { message; _ } = message

let split_at ~len:max str =
  if max <= 0 then invalid_arg "split_at";
  let dec = Uutf.decoder ~encoding:`UTF_8 (`String str) in
  let lines = ref [] and buf = Buffer.create 0x7ff and count = ref 0 in
  let flush () =
    lines := Buffer.contents buf :: !lines;
    Buffer.clear buf;
    count := 0
  in
  let add uchr =
    if !count >= max then flush ();
    Uutf.Buffer.add_utf_8 buf uchr;
    incr count
  in
  let rec go () =
    match Uutf.decode dec with
    | `Uchar u -> add u; go ()
    | `Malformed _ -> add Uutf.u_rep; go ()
    | `End -> flush ()
    | `Await -> assert false
  in
  go (); List.rev !lines

let split_at ~len { message; _ } = split_at ~len message
let make ~nickname message = { nickname; message }

let msgf ?(nickname = "Banawá") fmt =
  Fmt.kstr (fun message -> { nickname; message }) fmt
