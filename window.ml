open Nottui
open Notty

type t = { w : int; h : int; p : int }

let render_message ~width ~width_nicknames msg =
  let width_message =
    max 1 (width - width_nicknames - 1)
  in
  let message = Message.split_at ~len:width_message msg in
  let color = A.white in
  let rest =
    List.map @@ fun msg ->
    I.hcat
      [ I.void width_nicknames 1
      ; I.strf "│"
      ; I.strf "%s" msg
      ]
  in
  I.vcat
    (I.hcat
       [ I.strf " "
       ; I.hsnap ~align:`Right width_nicknames
           (I.strf ~attr:A.(fg color) "%s" (Message.nickname msg))
       ; I.strf "│"
       ; I.strf "%s" (List.hd message)
       ]
    :: rest (List.tl message))

let width_nicknames msgs =
  let f msg acc = max (String.length (Message.nickname msg)) acc in
  Rb.iter ~f msgs 0
 
let render { w; h; p } msgs =
  let idx = ref (Rb.length msgs - 1 - p) in
  let image = ref I.empty in
  let message = ref I.empty in
  let width_nicknames = width_nicknames msgs in
  while
    !idx >= 0
    &&
    (message :=
       render_message ~width_nicknames ~width:w msgs.Rb.%[!idx];
     I.height !message + I.height !image <= h)
  do
    (image := I.(!message <-> !image));
    decr idx
  done;
  Ui.atom (I.vsnap ~align:`Bottom h !image)

let handler ~hook:_ _state _buffer _key = `Unhandled

let make w =
  let ( let* ) x f = Lwd.bind ~f x in
  let ( let+ ) x f = Lwd.map ~f x in
  let ( and+ ) = Lwd.map2 ~f:(fun x y -> (x, y)) in

  let state = Lwd.var { w = 0; h = 0; p = 0 } in
  let hook = Lwd.set state in

  let* document =
    let+ state = Lwd.get state
    and+ buffer = Lwd.get w in
    Ui.keyboard_area
      (handler ~hook state buffer)
      (render state buffer)
  in

  let update_size ~w ~h =
    let state' = Lwd.peek state in
    if state'.w <> w || state'.h <> h then Lwd.set state { state' with w; h }
  in

  let measure_size document =
    Ui.size_sensor update_size (Ui.resize ~sh:1 document)
  in

  Lwd.return (measure_size document)
