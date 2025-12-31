(** The Backend module manages the server-side state logic. *)

module Document = Document

val document_state : Document.t ref
(** [document_state] is the authoritative source of truth for the document text.

    RI: It must only be modified via [apply_action] (or internal helpers) while
    holding [state_mutex] to ensure consistency with client cursors. *)

type client = {
  id : int;
  out_chan : Lwt_io.output_channel;
  cursor_obj : Document.cursor;
  mutex : Lwt_mutex.t;
}
(** AF: A record [c] represents a single active user session where: [c.id] is
    the unique session identifier. [c.out_chan] is the active TCP stream to the
    user. [c.cursor_obj] tracks where this user is currently editing in the
    document.

    RI: 1. [c.id] is equal to [c.cursor_obj.id]. 2. Concurrent writes to
    [c.out_chan] must be guarded by [c.mutex]. *)

val clients : client list ref
(** [clients] is the global list of currently connected users. RI: All clients
    in this list have unique [id]s. *)

val state_mutex : Lwt_mutex.t
(** [state_mutex] protects access to global mutable state. RI: This mutex must
    be locked before reading or writing [clients] or [document_state] to prevent
    race conditions. *)

val next_client_id : int ref
(** [next_client_id] tracks the next available unique ID. *)

val fresh_client_id : unit -> int
(** [fresh_client_id ()] Returns: A new integer ID that has never been issued
    before in this session. *)

val remove_client : client -> unit
(** [remove_client client] Effects: Removes [client] from the global [clients]
    list. *)

val save_to_disk : string -> string -> unit
(** [save_to_disk filename content] Effects: Writes [content] to the filesystem
    at [filename]. Used when a [SaveRequest] is received. *)

val string_of_user_action : Shared.Events.user_action -> string
(** Returns: A wire-protocol string representing [ua] (e.g., "KEY 97"). *)

val user_action_of_string : string -> Shared.Events.user_action option
(** Returns: [Some action] if the string is valid, or [None] if malformed. *)

val string_of_backend_update : Shared.Events.backend_update -> string
(** Returns: A wire-protocol string representing the server update. *)

val _backend_update_of_string : string -> Shared.Events.backend_update option
(** Returns: [Some update] if the string is valid, or [None]. *)

val apply_action :
  client -> Shared.Events.user_action -> Shared.Events.backend_update option
(** [apply_action client action] processes a single action.

    Effects: 1. If [SaveRequest], writes to disk via [save_to_disk]. 2. If edit
    action, mutates [document_state] and shifts other cursors.

    Returns: [Some update] if the change should be broadcast to other clients,
    or [None] if the action requires no broadcast. *)
