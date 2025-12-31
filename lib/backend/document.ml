(* lib/backend/document.ml *)

(* Open the shared types so we know what 'KeyPressed' and 'Backspace' are *)
open Shared.Events

(* THE STATE. Right now it's a string *)
type t = string

(* the cursor. mutable pos means we can update the position in place. *)
type cursor = {
  id : int;
  mutable pos : int;
}

(* Initialize an empty document *)
let create () : t = ""

(* Get the current text (for sending to new clients) *)
let to_string (doc : t) : string = doc

(* Get the length of the document *)
let length = String.length

(* --- HELPER FUNCTIONS --- *)

(* Keep a number between low and high limits *)
let clamp x ~lo ~hi = if x < lo then lo else if x > hi then hi else x

(* Insert a character into a string at a specific index *)
let insert_char (doc : string) (pos : int) (c : char) : string =
  let len = String.length doc in
  let safe_pos = clamp pos ~lo:0 ~hi:len in
  let before = String.sub doc 0 safe_pos in
  (* safe_pos is the split point. Length of 'after' is total - split point *)
  let after = String.sub doc safe_pos (len - safe_pos) in
  before ^ String.make 1 c ^ after

(* Delete the character BEFORE the cursor (Backspace behavior) *)
(* Returns: (new_string, index_of_deleted_char) *)
let delete_char_before (doc : string) (pos : int) : string * int option =
  let len = String.length doc in
  if pos <= 0 || len = 0 then (doc, None)
    (* Cannot delete if at start or empty *)
  else
    let valid_pos = clamp pos ~lo:1 ~hi:len in
    let delete_idx = valid_pos - 1 in
    let before = String.sub doc 0 delete_idx in
    let after = String.sub doc valid_pos (len - valid_pos) in
    (before ^ after, Some delete_idx)

(* --- MAIN LOGIC --- *)

(* apply_local_change ------------------ Inputs: - doc: The current text string.
   - action: What the user did (Key 'A', Backspace, etc). - actor_id: The ID of
   the user who performed the action. - cursors: A list of ALL connected cursors
   (so we can shift them). Returns: - The new document string. - A
   backend_update message (to broadcast to everyone), or None. *)
let apply_local_change (doc : t) (action : user_action) (actor_id : int)
    (cursors : cursor list) : t * backend_update option =
  (* Find the cursor of the person who triggered the action *)
  (* We use List.find_opt just in case something goes wrong *)
  let actor_cursor_opt = List.find_opt (fun c -> c.id = actor_id) cursors in

  match actor_cursor_opt with
  | None ->
      (* If the actor doesn't exist, don't change anything *)
      (doc, None)
  | Some actor -> (
      match action with
      (* CASE 1: The user typed a character *)
      | KeyPressed c ->
          let old_pos = actor.pos in

          (* update text *)
          let new_doc = insert_char doc old_pos c in

          (* Move the actor forward by 1 because they just typed *)
          actor.pos <- actor.pos + 1;

          (* Check everyone else. If they were AFTER the insertion point, push
             them to the right so they don't get overwritten. *)
          List.iter
            (fun c ->
              if c.id <> actor_id && c.pos >= old_pos then c.pos <- c.pos + 1)
            cursors;

          (new_doc, Some (DocumentChanged new_doc))
      (* CASE 2: The user hit Backspace *)
      | Backspace ->
          let old_pos = actor.pos in

          (* Update the text *)
          let new_doc, deleted_idx_opt = delete_char_before doc old_pos in

          begin match deleted_idx_opt with
          | None -> (doc, None) (* Nothing happened (e.g. at start of line) *)
          | Some deleted_idx ->
              (* Update the cursors *)
              actor.pos <- deleted_idx;

              (* Check everyone else. If they were to the right of the deleted
                 character, shift them left. *)
              List.iter
                (fun c ->
                  if c.id <> actor_id && c.pos > deleted_idx then
                    c.pos <- c.pos - 1)
                cursors;

              (new_doc, Some (DocumentChanged new_doc))
          end
      (* CASE 3: The user clicked somewhere (Set Cursor) *)
      | CursorSet new_pos ->
          let len = String.length doc in
          actor.pos <- clamp new_pos ~lo:0 ~hi:len;
          (doc, None)
      (* CASE 4: Save Request (handled by backend) *)
      | SaveRequest -> (doc, None)
      (* CASE 5: Alignment changes don't affect document state *)
      | AlignmentChanged _ -> (doc, None))
