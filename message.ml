type t = { nickname : string; message : string }

let nickname { nickname; _ } = nickname
let message { message; _ } = message

let split_at ~len:max str =
  let rec go acc off len =
    if len <= max then String.sub str off len :: acc
    else go (String.sub str off max :: acc) (off + max) (len - max)
  in
  if max <= 0 then invalid_arg "split_at";
  go [] 0 (String.length str) |> List.rev

let split_at ~len { message; _ } =
  split_at ~len message

let make ~nickname message = { nickname; message }

let msgf ?(nickname="Banawá") fmt =
  Fmt.kstr (fun message -> { nickname; message }) fmt
