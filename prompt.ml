open Nottui
open Notty

type t = {
  quit : unit -> unit;
  message : string -> unit;
  cursor : Rp.Cursor.cursor;
}

let make quit message =
  let cursor = Rp.Cursor.create Rp.empty 0 in
  { quit; message; cursor }

let map_cursor f state =
  { state with cursor = f state.cursor }

module Utils = struct
  let move_cursor ?(visual = true) ~hook cursor = function
    | `Left ->
        let position = Rp.Cursor.position cursor in
        (if position > 0 then
           let cursor = Rp.Cursor.move_backward cursor 1 in
           hook cursor);
        `Handled
    | `Right ->
        let position = Rp.Cursor.position cursor in
        let rope = Rp.Cursor.to_rope cursor in
        let len = Rp.length rope in
        let len = if visual then len - 1 else len in
        (if position < len then
           let cursor = Rp.Cursor.move_forward cursor 1 in
           hook cursor);
        `Handled

  let is_print = function '\x21' .. '\x7e' | ' ' -> true | _ -> false

  let render_cursor ~width cursor =
    let rope = Rp.Cursor.to_rope cursor in
    let position = Rp.Cursor.position cursor in
    let length = Rp.length rope in
    let offset = if position >= width then position - width else 0 in
    let rope = Rp.sub rope offset (length - offset) in
    (* XXX(dinosaure): shift our text according to [offset]. *)
    let length = Rp.length rope in
    let left, middle, right =
      match position >= 0 && position < length with
      | true ->
          ( Rp.sub rope 0 position
          , Some (Rp.get rope position)
          , Rp.sub rope (position + 1) (length - position - 1) )
      | false -> (rope, None, Rp.empty)
    in
    let middle =
      match middle with
      | None -> I.uchar A.empty (Uchar.of_char ' ') 1 1
      | Some uchar -> I.uchar A.empty uchar 1 1
    in
    ( I.hcat [ I.strf "%a" Rp.print left; middle; I.strf "%a" Rp.print right ]
    , position - offset )
end

module User_prompt = struct
  let render ~cursor ~y ~w state =
    let text, position =
      Utils.render_cursor ~width:(max 0 (w - 3)) state.cursor
    in
    Lwd.set cursor (position + 1, y);
    I.hcat [ I.char A.empty ' ' 1 1 ; text ]
end

let handler ~hook state = function
  | `ASCII chr, [] when Utils.is_print chr ->
    map_cursor (fun cursor ->
        let cursor = Rp.Cursor.insert_char cursor (Uchar.of_char chr) in
        Rp.Cursor.move_forward cursor 1)
      state
    |> hook;
    `Handled
  | `Uchar uchar, [] ->
    map_cursor (fun cursor ->
        let cursor = Rp.Cursor.insert_char cursor uchar in
        Rp.Cursor.move_forward cursor 1)
      state
    |> hook;
    `Handled
  | `Backspace, [] ->
    if Rp.Cursor.position state.cursor > 0 then
      map_cursor (fun cursor ->
          let cursor = Rp.Cursor.move_backward cursor 1 in
          Rp.Cursor.delete cursor)
        state
      |> hook;
    `Handled
  | `Arrow (`Left | `Right as direction), [] ->
    let hook cursor = hook { state with cursor } in
    Utils.move_cursor ~visual:false ~hook state.cursor direction
  | `Enter, [] ->
    let rope = Rp.Cursor.to_rope state.cursor in
    let msg =
      let len = Rp.length rope in
      let buf = Buffer.create len in
      Rp.iter_range (Uutf.Buffer.add_utf_8 buf) rope 0 len;
      Buffer.contents buf
    in
    state.message msg;
    hook { state with cursor = Rp.Cursor.create Rp.empty 0 };
    `Handled
  | `ASCII ('C'..'D'), [`Ctrl] ->
    state.quit ();
    `Handled
  | _ -> `Unhandled

let make ~quit ~message cursor =
  let ( let* ) x f = Lwd.bind x ~f in
  let ( let+ ) x f = Lwd.map ~f x in
  let ( and+ ) = Lwd.map2 ~f:(fun x y -> (x, y)) in
  let state = Lwd.var (make quit message) in
  let position = Lwd.var (0, 0) in
  let hook = Lwd.set state in
  let update_prompt state (y, w) =
    let user = User_prompt.render ~cursor ~y ~w state in
    Ui.keyboard_area (handler ~hook state) (Ui.atom user)
  in
  let update_position ~x:_ ~y ~w ~h:_ () =
    let y', w' = Lwd.peek position in
    if y' <> y || w' <> w then Lwd.set position (y, w)
  in
  let* prompts =
    let+ state = Lwd.get state
    and+ position = Lwd.get position in
    update_prompt state position
  in
  Lwd.return (Ui.transient_sensor update_position prompts)
