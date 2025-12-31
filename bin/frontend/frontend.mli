type alignment =
  | Left
  | Center
  | Right
      (**Abstraction Function: [Left] represents text aligned to the left
         margin. [Center] represents text centered in the window. [Right]
         represents text aligned to the right margin.*)

type client_state = {
  mutable document : string;
  mutable cursor_pos : int;
  sock : Unix.file_descr;
  in_chan : in_channel;
  out_chan : out_channel;
  mutex : Mutex.t;
  mutable alignment : alignment;
  mutable dirty : bool;
}
(** Client state maintained by the GUI client.

    AF: A record [s] represents an active editing session where: [s.document] is
    the local replica of the shared text. [s.cursor_pos] is the index where the
    user's next keystroke will be inserted. [s.alignment] is the user's current
    visual preference. [s.dirty] is a flag indicating if the view is out of sync
    with the data. [s.sock], [in_chan], and [out_chan] represent the active TCP
    connection stream to the server.

    RI: 1. [cursor_pos] is always within the bounds of [document] (0 <=
    cursor_pos <= String.length document). 2. The [in_chan] and [out_chan]
    correspond to the valid, open file descriptor [sock]. 3. [mutex] must be
    locked before accessing or modifying [document], [cursor_pos], or [dirty] to
    prevent race conditions between the GUI and network threads. 4. If [dirty]
    is false, the GUI display matches the current [document] state.

    Fields: [document]: current document text known to the client. [cursor_pos]:
    locally tracked cursor position. [sock], [in_chan], [out_chan]: socket and
    I/O channels for the server connection. [mutex]: protects writes to
    [out_chan]. [alignment]: display alignment used by this client
    (left/center/right). *)

val string_of_user_action : Shared.Events.user_action -> string
(** [string_of_user_action ua] Return value: a wire-protocol string representing
    [ua] (no trailing newline). Examples: ["KEY 97"], ["BACKSPACE"]. *)

val backend_update_of_string : string -> Shared.Events.backend_update option
(** [backend_update_of_string s]

    Return value: [Some update] when [s] encodes a known backend update (e.g.
    "FULL <doc>" or "DOC <doc>"); otherwise [None]. *)

val send_action : client_state -> Shared.Events.user_action -> unit
(** [send_action state action] produce a side effects: write a line encoding
    [action] to [state.out_chan] and flush. This function serializes writes
    using [state.mutex]. *)

val receive_updates : client_state -> unit
(** [receive_updates state] read lines from [state.in_chan], parse them, and
    update [state]. Intended to run in a background thread. (blocking)*)

val connect_to_server : string -> int -> client_state
(** [connect_to_server ip port] Connect to [ip]:[port] and return an initialized
    [client_state]. May raise Unix exceptions on failure. *)

val run_gui_client : string -> int -> unit
(** [run_gui_client ip port] Start the GUI client, connect it to [ip]:[port],
    and enter the Bogue mainloop. Returns when the GUI exits. *)
