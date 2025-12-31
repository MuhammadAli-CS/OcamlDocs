(* lib/shared/events.ml *)

(** Shared types between frontend and backend. *)

(* What the GUI / client sends to the backend *)
type user_action =
  | KeyPressed of char (* insert this character at the cursor *)
  | Backspace (* delete character before the cursor *)
  | CursorSet of int (* set cursor to absolute index in the doc *)
  | SaveRequest (* trigger save (handled elsewhere) *)
  | AlignmentChanged of
      string (* change text alignment: "left", "center", or "right" *)

(* What the backend sends to GUI / clients *)
type backend_update =
  | FullDocument of string (* full snapshot on connect or reload *)
  | DocumentChanged of string (* full document after an edit *)
  | CursorMoved of int (* update client's cursor position *)
  | AlignmentUpdate of string (* broadcast alignment change to all clients *)
