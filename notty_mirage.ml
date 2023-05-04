let src = Logs.Src.create "notty.mirage"

module Log = (val Logs.src_log src : Logs.LOG)
open Lwt.Infix
open Notty

let ( </> ) a b = Lwt.pick [ a >|= Either.left; b >|= Either.right ]
let ( <??> ) a b = a >|= Either.left <?> (b >|= Either.right)

module Make (Time : Mirage_time.S) = struct
  module Lwt_condition = struct
    include Lwt_condition

    let map f v =
      let v' = create () in
      let rec go () =
        wait v >>= fun x ->
        broadcast v' (f x);
        go ()
      in
      Lwt.async go;
      v'

    let unburst ~timeout v =
      let v' = create () in
      let rec delay x =
        Time.sleep_ns timeout </> wait v >>= function
        | Either.Left () ->
            broadcast v' x;
            start ()
        | Either.Right x -> delay x
      and start () = wait v >>= delay in
      Lwt.async start;
      v'
  end

  module Term = struct
    let input_stream ic stop =
      let flt = Unescape.create () in
      let ibuf = Bytes.create 1024 in
      let rec next () =
        match Unescape.next flt with
        | #Unescape.event as r -> Lwt.return_some r
        | `End -> Lwt.return_none
        | `Await -> (
            ic () <??> stop >>= function
            | Either.Right _ -> Lwt.return_none
            | Either.Left `Eof ->
                Unescape.input flt ibuf 0 0;
                next ()
            | Either.Left (`Data cs) ->
                let rec go cs =
                  if Cstruct.length cs > 0 then (
                    let len = min (Bytes.length ibuf) (Cstruct.length cs) in
                    Cstruct.blit_to_bytes cs 0 ibuf 0 len;
                    Unescape.input flt ibuf 0 len;
                    go (Cstruct.shift cs len))
                  else Lwt.return_unit
                in
                go cs >>= next)
      in
      Lwt_stream.from next

    type t =
      { oc : Cstruct.t -> unit Lwt.t
      ; trm : Notty.Tmachine.t
      ; buf : Buffer.t
      ; events : [ Unescape.event | `Resize of int * int ] Lwt_stream.t
      ; stop : unit -> unit
      }

    let write t =
      Tmachine.output t.trm t.buf;
      let out = Buffer.contents t.buf in
      Buffer.clear t.buf;
      t.oc (Cstruct.of_string out)

    let refresh t =
      Tmachine.refresh t.trm;
      write t

    let image t image =
      Tmachine.image t.trm image;
      write t

    let cursor t curs =
      Tmachine.cursor t.trm curs;
      write t

    let set_size t dim = Tmachine.set_size t.trm dim
    let size t = Tmachine.size t.trm

    let release t =
      if Tmachine.release t.trm then (
        t.stop ();
        write t (* TODO(dinosaure): send [`Eof] *))
      else Lwt.return_unit

    let resize dim stop on_resize =
      (* TODO(dinosaure): we can save some allocations here but I mostly followed `notty-lwt`. *)
      let rcond =
        Lwt_condition.unburst ~timeout:1000L dim
        |> Lwt_condition.map Option.some
      in
      let rec monitor () =
        Lwt_condition.wait rcond <?> stop >>= function
        | Some dim ->
            on_resize dim;
            monitor ()
        | None -> Lwt.return_unit
      in
      Lwt.dont_wait monitor (fun exn ->
          Logs.err @@ fun m ->
          m "Got an exception from the resizer: %S" (Printexc.to_string exn));
      Lwt_stream.from (fun () -> Lwt_condition.wait rcond <?> stop)
      |> Lwt_stream.map (fun dim -> `Resize dim)

    let create ?(dispose = true) ?(bpaste = true) ?(mouse = true)
        (size, sigwinch) ic oc =
      let stop, do_stop = Lwt.wait () in
      let rec t =
        lazy
          { trm =
              Tmachine.create ~mouse ~bpaste
                Cap.ansi (* XXX(dinosaure): we assume! *)
          ; oc
          ; buf = Buffer.create 4096
          ; stop = (fun () -> Lwt.wakeup do_stop None)
          ; events =
              Lwt_stream.choose
                [ input_stream ic stop
                ; ( resize sigwinch stop @@ fun dim ->
                    let t = Lazy.force t in
                    Buffer.reset t.buf;
                    set_size t dim )
                ]
          }
      in
      let t = Lazy.force t in
      set_size t size;
      Lwt.async (fun () -> write t);
      if dispose then Mirage_runtime.at_exit (fun () -> release t);
      t

    let events t = t.events
  end
end
