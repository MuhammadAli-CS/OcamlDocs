(** Shared types between frontend and backend. *)

(* What the GUI / client sends to the backend *)
type user_action =
  | KeyPressed of char  (** Insert this character at the cursor *)
  | Backspace  (** Delete character before the cursor *)
  | CursorSet of int  (** Set cursor to absolute index in the document *)
  | SaveRequest  (** Trigger save (handled elsewhere) *)
  | AlignmentChanged of string

(* What the backend sends to GUI / clients *)
type backend_update =
  | FullDocument of string  (** Full snapshot on connect or reload *)
  | DocumentChanged of string  (** Full document after an edit *)
  | CursorMoved of int  (** Update client's cursor position *)
  | AlignmentUpdate of string  (** Broadcast alignment change to all clients *)
