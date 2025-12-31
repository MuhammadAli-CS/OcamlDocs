(* lib/backend/server.ml *)
open Lwt.Infix
module Document = Document

let document_state : Document.t ref = ref (Document.create ())

type client = {
  id : int;
  out_chan : Lwt_io.output_channel;
  cursor_obj : Document.cursor;
  mutex : Lwt_mutex.t;
}

let clients : client list ref = ref []
let state_mutex = Lwt_mutex.create ()
let next_client_id = ref 0

let fresh_client_id () =
  let id = !next_client_id in
  incr next_client_id;
  id

let remove_client (cl : client) =
  clients := List.filter (fun c -> c.id <> cl.id) !clients

let save_to_disk filename content =
  try
    let oc = open_out filename in
    output_string oc content;
    close_out oc;
    Printf.printf "Successfully saved document to %s\n%!" filename
  with e -> Printf.printf "Failed to save file: %s\n%!" (Printexc.to_string e)

let string_of_user_action (ua : Shared.Events.user_action) : string =
  match ua with
  | Shared.Events.KeyPressed c -> Printf.sprintf "KEY %d" (Char.code c)
  | Shared.Events.Backspace -> "BACKSPACE"
  | Shared.Events.CursorSet pos -> Printf.sprintf "SET %d" pos
  | Shared.Events.SaveRequest -> "SAVE"
  | Shared.Events.AlignmentChanged align -> "ALIGN " ^ align

let user_action_of_string (s : string) : Shared.Events.user_action option =
  let trimmed = String.trim s in
  match String.split_on_char ' ' trimmed with
  | [ "BACKSPACE" ] -> Some Shared.Events.Backspace
  | [ "SAVE" ] -> Some Shared.Events.SaveRequest
  | [ "ALIGN"; align_str ] -> Some (Shared.Events.AlignmentChanged align_str)
  | [ "KEY"; code_str ] -> (
      match int_of_string_opt code_str with
      | None -> None
      | Some n ->
          if n < 0 || n > 255 then None
          else Some (Shared.Events.KeyPressed (Char.chr n)))
  | [ "SET"; pos_str ] -> (
      match int_of_string_opt pos_str with
      | None -> None
      | Some p -> Some (Shared.Events.CursorSet p))
  | _ -> None

let string_of_backend_update (up : Shared.Events.backend_update) : string =
  match up with
  | Shared.Events.FullDocument doc -> "FULL " ^ doc
  | Shared.Events.DocumentChanged doc -> "DOC " ^ doc
  | Shared.Events.CursorMoved pos -> "CURSOR " ^ string_of_int pos
  | Shared.Events.AlignmentUpdate align -> "ALIGN " ^ align

let _backend_update_of_string (s : string) : Shared.Events.backend_update option
    =
  let len = String.length s in
  if len >= 5 && String.sub s 0 5 = "FULL " then
    let doc = String.sub s 5 (len - 5) in
    Some (Shared.Events.FullDocument doc)
  else if len >= 4 && String.sub s 0 4 = "DOC " then
    let doc = String.sub s 4 (len - 4) in
    Some (Shared.Events.DocumentChanged doc)
  else None

let apply_action (cl : client) (ua : Shared.Events.user_action) :
    Shared.Events.backend_update option =
  match ua with
  | Shared.Events.SaveRequest ->
      let content = Document.to_string !document_state in
      save_to_disk "data/saveddoc.txt" content;
      None
  | Shared.Events.AlignmentChanged align ->
      Some (Shared.Events.AlignmentUpdate align)
  | _ ->
      let all_cursors = List.map (fun c -> c.cursor_obj) !clients in
      let new_doc, update_opt =
        Document.apply_local_change !document_state ua cl.id all_cursors
      in
      document_state := new_doc;
      update_opt
