open Lwt.Infix
open Backend

(* Shortcuts to make code more readable *)
module Server = Server
module Document = Document
module Events = Shared.Events

(* Broadcast a general message (like "DOC ...") to all connected clients *)
let broadcast_change (update : Events.backend_update) : unit Lwt.t =
  let msg = Server.string_of_backend_update update in

  (* Helper to send to one client safely *)
  let send out_chan client_mutex =
    Lwt.catch
      (fun () ->
        Lwt_mutex.with_lock client_mutex (fun () ->
            Lwt_io.write_line out_chan msg >>= fun () -> Lwt_io.flush out_chan))
      (function
        | Lwt_io.Channel_closed _ | Unix.Unix_error (Unix.EPIPE, _, _) ->
            (* If a client disconnected, just ignore the error here. The read
               loop will catch it and remove them properly. *)
            Lwt.return_unit
        | exn -> Lwt.fail exn)
  in
  (* Send to everyone in parallel *)
  Lwt_list.iter_p
    (fun c -> send c.Server.out_chan c.Server.mutex)
    !Server.clients

(* Send the initial document state to a newly connected client *)
let send_full_document_to (cl : Server.client) : unit Lwt.t =
  let doc_str = Document.to_string !Server.document_state in
  let update = Events.FullDocument doc_str in
  let msg = Server.string_of_backend_update update in

  Lwt_mutex.with_lock cl.mutex (fun () ->
      Lwt_io.write_line cl.out_chan msg >>= fun () -> Lwt_io.flush cl.out_chan)

let handle_client (_addr : Unix.sockaddr) (in_ch, out_ch) : unit Lwt.t =
  (* 1. Initialize Client State *)
  let my_id = Server.fresh_client_id () in
  let my_cursor = { Document.id = my_id; pos = 0 } in
  let cl : Server.client =
    {
      id = my_id;
      out_chan = out_ch;
      cursor_obj = my_cursor;
      mutex = Lwt_mutex.create ();
    }
  in

  (* 2. Handshake: Add to global list, set cursor to end, send Doc *)
  Lwt_mutex.with_lock Server.state_mutex (fun () ->
      let len = Document.length !Server.document_state in
      my_cursor.pos <- len;
      Server.clients := cl :: !Server.clients;
      send_full_document_to cl)
  >>= fun () ->
  (* 3. The Infinite Read Loop *)
  let rec loop () =
    Lwt_io.read_line_opt in_ch >>= function
    | None ->
        (* Client disconnected (End of File) *)
        Lwt_mutex.with_lock Server.state_mutex (fun () ->
            Server.remove_client cl;
            Lwt.return_unit)
    | Some line -> (
        match Server.user_action_of_string line with
        | None ->
            (* Malformed message, ignore and keep listening *)
            loop ()
        | Some ua ->
            (* PROCESS THE ACTION *)
            (* We lock the STATE mutex because apply_action mutates the document/cursors *)
            Lwt_mutex.with_lock Server.state_mutex (fun () ->
                (* A. Apply the logic (Edit text, save file, etc.) *)
                let up = Server.apply_action cl ua in

                (* B. Snapshot all cursor positions immediately. We need to tell
                   every client where their cursor ended up after the shift
                   logic ran. *)
                let updates =
                  List.map
                    (fun c -> (c, c.Server.cursor_obj.pos))
                    !Server.clients
                in
                Lwt.return (up, updates))
            >>= fun (broadcast_opt, cursor_updates) ->
            (* C. Broadcast main update (e.g., "DOC new_text") *)
            (match broadcast_opt with
              | None -> Lwt.return_unit
              | Some up -> broadcast_change up)
            >>= fun () ->
            (* D. Send specific cursor updates to each client (e.g. Tell Client
               A: "Your cursor is now at 5") *)
            Lwt_list.iter_p
              (fun (c, pos) ->
                let msg =
                  Server.string_of_backend_update (Events.CursorMoved pos)
                in
                Lwt.catch
                  (fun () ->
                    Lwt_mutex.with_lock c.Server.mutex (fun () ->
                        Lwt_io.write_line c.Server.out_chan msg >>= fun () ->
                        Lwt_io.flush c.out_chan))
                  (fun _ -> Lwt.return_unit))
              cursor_updates
            >>= fun () -> loop ())
  in
  loop ()

let start_server (ip : string) (port : int) : unit Lwt.t =
  let inet = Unix.inet_addr_of_string ip in
  let sockaddr = Unix.ADDR_INET (inet, port) in

  (* Setup Ctrl-C (SIGINT) handling to exit gracefully *)
  let waiter, wakener = Lwt.wait () in
  let sigint_handler =
    Lwt_unix.on_signal Sys.sigint (fun _ ->
        Printf.printf "\nReceived Ctrl-C, shutting down server...\n%!";
        Lwt.wakeup wakener ())
  in

  Lwt_io.establish_server_with_client_address sockaddr handle_client
  >>= fun _server ->
  Printf.printf "Server started on %s:%d\n%!" ip port;
  Printf.printf "Press Ctrl-C to stop the server\n%!";

  (* Block here until Ctrl-C is pressed *)
  waiter >>= fun () ->
  Lwt_unix.disable_signal_handler sigint_handler;
  Printf.printf "Server stopped.\n%!";
  Lwt.return_unit

let () =
  (* Run the Lwt main loop *)
  Lwt_main.run (start_server "127.0.0.1" 9000)
