open Shared.Events
(**for appply_local_change function data type which needs this*)

type t = string
(** AF: the entire document text as an immutable sequence of characters
    (string). The abstract document is the sequence of characters stored in the
    string. RI: any doc: t is a valid string and contains no newline character.
*)

type cursor = {
  id : int;
  mutable pos : int;
}
(** AF: The type representing a connected user's cursor. This is exposed so the
    Backend can manage client state. RI: Cursor position must be within document
    bounds 0 <= cursor.pos <= String.length doc*)

val create : unit -> t
(** [create ()] initializes a new, empty document. *)

val to_string : t -> string
(** [to_string doc] returns the raw string content of the document. *)

val length : t -> int
(** [length doc] returns the current length of the document. *)

val insert_char : t -> int -> char -> t
(** [insert_char doc pos c] inserts a char at a position.(exposed for testing)
*)

val delete_char_before : t -> int -> t * int option
(** [delete_char_before doc pos] deletes the char before the position.(exposed
    for testing)*)

val apply_local_change :
  t -> user_action -> int -> cursor list -> t * backend_update option
(** [apply_local_change doc action actor_id cursors] applies a single user
    action (insert, delete, move) to the document state. [doc]: The current
    document state. [action]: The action performed (KeyPressed, Backspace,
    etc.). [actor_id]: The ID of the client performing the action. [cursors]: A
    list of ALL active cursors (needed for shifting logic). mutates the [pos]
    field of cursors in the [cursors] list to keep them in sync with text
    changes. returns A tuple containing the new document state and an optional
    [backend_update] to broadcast to other clients. *)
