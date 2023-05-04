include Rope.Make_array (struct
  include Uchar

  let uchar_to_utf_8 =
    let buf = Buffer.create 16 in
    fun uchar ->
      Uutf.Buffer.add_utf_8 buf uchar;
      let res = Buffer.contents buf in
      Buffer.clear buf;
      res

  let print =
    Fmt.if_utf_8
      Fmt.(using uchar_to_utf_8 string)
      Fmt.(using Uchar.to_int (any "U+04X"))
end)

let to_utf_8_string rope =
  let len = length rope in
  let buf = Buffer.create len in
  iter_range (Uutf.Buffer.add_utf_8 buf) rope 0 len;
  Buffer.contents buf

let of_utf_8_string str =
  Uutf.String.fold_utf_8
    (fun (rope, upos) _bpos -> function
      | `Malformed _ -> (insert_char rope upos Uutf.u_rep, succ upos)
      | `Uchar uchr -> (insert_char rope upos uchr, succ upos))
    (empty, 0) str
  |> fst
