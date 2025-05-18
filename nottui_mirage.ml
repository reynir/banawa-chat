let src = Logs.Src.create "nottui.mirage"

module Log = (val Logs.src_log src : Logs.LOG)

module Make = struct
  open Notty_mirage.Make

  let copy_until quit ~f input =
    let quit = Lwt.map (fun () -> None) quit in
    let stream, push = Lwt_stream.create () in
    let rec go () =
      Lwt.bind (Lwt.choose [ quit; Lwt_stream.peek input ]) @@ function
      | None ->
          push None;
          Lwt.return_unit
      | Some x ->
          push (Some (f x));
          Lwt.bind (Lwt_stream.junk input) go
    in
    Lwt.async go;
    stream

  let render ?quit ~size events document =
    let renderer = Nottui.Renderer.make () in
    let refresh_stream, push_refresh = Lwt_stream.create () in
    let root =
      Lwd.observe
        ~on_invalidate:(fun _ ->
          if not (Lwt_stream.is_closed refresh_stream) then
            push_refresh (Some ()))
        document
    in
    let quit, _do_quit =
      match quit with
      | Some quit -> (quit, None)
      | None ->
          let t, u = Lwt.wait () in
          (t, Some u)
    in
    let events =
      copy_until quit events ~f:(fun e ->
          (e
            : [ `Resize of _ | Notty.Unescape.event ]
            :> [ `Resize of _ | Nottui.Ui.event ]))
    in
    let size = ref size in
    let result, push = Lwt_stream.create () in
    let refresh () =
      let ui = Lwd.quick_sample root in
      Nottui.Renderer.update renderer !size ui;
      push (Some (Nottui.Renderer.image renderer))
    in
    refresh ();
    let process_event = function
      | #Nottui.Ui.event as event ->
          ignore (Nottui.Renderer.dispatch_event renderer event)
      | `Resize size' ->
          size := size';
          refresh ()
    in
    Lwt.async (fun () ->
        Lwt.finalize
          (fun () -> Lwt_stream.iter process_event events)
          (fun () ->
            push None;
            Lwt.return_unit));
    Lwt.async (fun () -> Lwt_stream.iter refresh refresh_stream);
    result

  let run ?cursor ?quit (size, sigwinch) document ic oc =
    let term = Term.create (size, sigwinch) ic oc in
    let images = render ?quit ~size (Term.events term) document in
    let cursor () =
      let cursor =
        cursor
        |> Option.map @@ fun cursor ->
           Lwd.quick_sample (Lwd.observe (Lwd.get cursor))
      in
      Term.cursor term cursor
    in
    Lwt.finalize
      (fun () ->
        Lwt_stream.iter_s
          (fun image -> Lwt.join [ Term.image term image; cursor () ])
          images)
      (fun () -> Term.release term)
end
