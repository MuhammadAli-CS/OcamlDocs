(* lib/frontend/frontend.ml *)

open Shared.Events
open Tsdl

type document_theme = {
  text : Bogue.Draw.rgb;
  background : Bogue.Draw.color;
}

let color r g b a = Sdl.Color.create ~r ~g ~b ~a

(* Theme definitions *)

let default_theme = { text = (0, 0, 0); background = (255, 255, 255, 255) }
let light_theme = { text = (50, 50, 50); background = (240, 240, 240, 255) }
let dark_theme = { text = (220, 220, 220); background = (30, 30, 30, 255) }
let bright_theme = { text = (255, 255, 102); background = (211, 211, 211, 255) }

type alignment =
  | Left
  | Center
  | Right

let alignment_of_string (s : string) : alignment =
  match String.lowercase_ascii (String.trim s) with
  | "left" -> Left
  | "right" -> Right
  | "center" | "centre" -> Center
  | _ -> Left

let alignment_from_env () : alignment =
  match Sys.getenv_opt "EDITOR_ALIGN" with
  | Some v -> alignment_of_string v
  | None -> Left

let wrap_string (s : string) (width : int) : string list =
  let len = String.length s in
  if len <= width then [ s ]
  else
    let rec aux start_idx =
      if start_idx >= len then []
      else
        let remaining = len - start_idx in
        if remaining <= width then [ String.sub s start_idx remaining ]
        else
          (* Find split point *)
          let split_idx = ref (start_idx + width) in
          while !split_idx > start_idx && s.[!split_idx] <> ' ' do
            decr split_idx
          done;
          if !split_idx = start_idx then split_idx := start_idx + width;

          (* Force split if no space *)
          let sub_len = !split_idx - start_idx in
          let line = String.sub s start_idx sub_len in
          let next_start =
            if !split_idx < len && s.[!split_idx] = ' ' then !split_idx + 1
            else !split_idx
          in
          line :: aux next_start
    in
    aux 0

let apply_alignment (align : alignment) (doc : string) : string =
  match align with
  | Left ->
      (* Even for Left alignment, we should wrap to prevent crashes on long
         lines *)
      let lines = String.split_on_char '\n' doc in
      let wrapped_lines =
        List.concat (List.map (fun line -> wrap_string line 80) lines)
      in
      String.concat "\n" wrapped_lines
  | (Center | Right) as a ->
      (* Align each explicit line (split on '\n'). We only change what we draw,
         never the document stored in [state.document] or on the server. *)
      let lines = String.split_on_char '\n' doc in
      (* Use a fixed width for alignment (approx 80 chars for 600px width) *)
      let max_len = 80 in

      let wrapped_lines =
        List.concat (List.map (fun line -> wrap_string line max_len) lines)
      in

      let pad_line line =
        let len = String.length line in
        if len = 0 then " " (* Prevent empty lines which crash SDL *)
        else
          let pad =
            match a with
            | Right -> max 0 (max_len - len)
            | Center -> max 0 ((max_len - len) / 2)
            | Left -> 0
          in
          if pad = 0 then line else String.make pad ' ' ^ line
      in
      let res = String.concat "\n" (List.map pad_line wrapped_lines) in
      res

(* caret display removed — render document string directly *)

(* Insert a visual caret character into a copy of [doc] at [pos]. This returns a
   new string; it does not mutate the real document stored in [state.document].
   We use a simple pipe character '|' as the caret. *)
let insert_caret_at (doc : string) (pos : int) : string =
  let len = String.length doc in
  let safe_pos = if pos < 0 then 0 else if pos > len then len else pos in
  let before = if safe_pos = 0 then "" else String.sub doc 0 safe_pos in
  let after =
    if safe_pos >= len then "" else String.sub doc safe_pos (len - safe_pos)
  in
  before ^ "|" ^ after

(* Client state *)
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

(* Wire protocol - matching backend *)
let string_of_user_action (ua : user_action) : string =
  match ua with
  | KeyPressed c -> Printf.sprintf "KEY %d" (Char.code c)
  | Backspace -> "BACKSPACE"
  | CursorSet pos -> Printf.sprintf "SET %d" pos
  | SaveRequest -> "SAVE"
  | AlignmentChanged align -> "ALIGN " ^ align

let backend_update_of_string (s : string) : backend_update option =
  let len = String.length s in
  if len >= 5 && String.sub s 0 5 = "FULL " then
    let doc = String.sub s 5 (len - 5) in
    Some (FullDocument doc)
  else if len >= 4 && String.sub s 0 4 = "DOC " then
    let doc = String.sub s 4 (len - 4) in
    Some (DocumentChanged doc)
  else if len >= 7 && String.sub s 0 7 = "CURSOR " then
    let pos_str = String.sub s 7 (len - 7) in
    match int_of_string_opt pos_str with
    | Some pos -> Some (CursorMoved pos)
    | None -> None
  else if len >= 6 && String.sub s 0 6 = "ALIGN " then
    let align_str = String.sub s 6 (len - 6) in
    Some (AlignmentUpdate align_str)
  else None

(* Send an action to the server (blocking, thread-safe) *)
let send_action (state : client_state) (action : user_action) : unit =
  Mutex.lock state.mutex;
  let msg = string_of_user_action action in
  (* Debug: print outgoing actions to stdout so ordering can be observed. *)
  Printf.printf "SEND: %s\n%!" msg;
  output_string state.out_chan msg;
  output_char state.out_chan '\n';
  flush state.out_chan;
  Mutex.unlock state.mutex

(* Background thread to receive updates from server *)
let rec receive_updates (state : client_state) : unit =
  try
    let line = input_line state.in_chan in
    (match backend_update_of_string line with
    | Some (FullDocument doc) | Some (DocumentChanged doc) ->
        Mutex.lock state.mutex;
        state.document <- doc;
        state.dirty <- true;
        Mutex.unlock state.mutex
    | Some (CursorMoved pos) ->
        Mutex.lock state.mutex;
        state.cursor_pos <- pos;
        state.dirty <- true;
        Mutex.unlock state.mutex
    | Some (AlignmentUpdate align_str) ->
        Mutex.lock state.mutex;
        state.alignment <- alignment_of_string align_str;
        state.dirty <- true;
        Mutex.unlock state.mutex
    | None -> ());

    receive_updates state
  with
  | End_of_file -> ()
  | e ->
      Printf.eprintf "Error receiving updates: %s\n" (Printexc.to_string e);
      ()

(* Connect to server using basic Unix sockets *)
let connect_to_server (ip : string) (port : int) : client_state =
  let inet = Unix.inet_addr_of_string ip in
  let sockaddr = Unix.ADDR_INET (inet, port) in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.connect sock sockaddr;
  let in_chan = Unix.in_channel_of_descr sock in
  let out_chan = Unix.out_channel_of_descr sock in
  {
    document = "";
    cursor_pos = 0;
    sock;
    in_chan;
    out_chan;
    mutex = Mutex.create ();
    alignment = alignment_from_env ();
    dirty = true;
  }

(* Create and run the Bogue GUI *)
let run_gui_client (ip : string) (port : int) : unit =
  print_endline "Connecting to server...";

  (* Connect to server *)
  let state = connect_to_server ip port in
  print_endline "Connected!";

  (* Create widgets *)
  (* Use a non-empty initial string and fix the widget width/height so wrapping
     is always computed with a valid, non-zero width. *)
  let base_doc = if state.document = "" then " " else state.document in
  let initial_doc = apply_alignment state.alignment base_doc in
  let text_display = Bogue.Widget.text_display ~w:600 ~h:300 initial_doc in

  (* Keep an input buffer widget but make it effectively invisible so users type
     'into' the displayed text. Polling will still read this buffer and send
     key/backspace actions to the server. *)
  let input_field =
    Bogue.Widget.text_input ~max_size:1000 ~prompt:"Type here to edit..." ()
  in

  let status_label = Bogue.Widget.button "Connected to server" in

  (* Button to send typed text as individual key presses *)
  let send_button = Bogue.Widget.button "Send Text" in
  let on_send_click _ =
    let text = Bogue.Widget.get_text input_field in
    if text <> "" then begin
      String.iter (fun c -> send_action state (KeyPressed c)) text;
      (* Clear the field AFTER the sends are queued *)
      Bogue.Widget.set_text input_field ""
    end
  in
  Bogue.Widget.on_click ~click:on_send_click send_button;

  (* Backspace button *)
  let backspace_button = Bogue.Widget.button "Backspace" in
  let on_backspace_click _ = send_action state Backspace in
  Bogue.Widget.on_click ~click:on_backspace_click backspace_button;

  (* Save button *)
  let save_button = Bogue.Widget.button "Save" in
  let on_save_click _ =
    send_action state SaveRequest;
    Bogue.Widget.set_text status_label "Save requested!"
  in
  Bogue.Widget.on_click ~click:on_save_click save_button;

  (* Focus button removed — use the visible input field below for focus *)

  (* Left button *)
  let left_button = Bogue.Widget.button "<" in
  let on_left_click _ =
    let new_pos = max 0 (state.cursor_pos - 1) in
    (* Update local cursor_pos immediately so subsequent key sends use the
       intended position before the server echoes back the cursor update. *)
    state.cursor_pos <- new_pos;
    send_action state (CursorSet new_pos)
  in
  Bogue.Widget.on_click ~click:on_left_click left_button;

  (* Right button *)
  let right_button = Bogue.Widget.button ">" in
  let on_right_click _ =
    let len = String.length state.document in
    let new_pos = min len (state.cursor_pos + 1) in
    state.cursor_pos <- new_pos;
    send_action state (CursorSet new_pos)
  in
  Bogue.Widget.on_click ~click:on_right_click right_button;

  (* Helper to update the text display based on current state *)
  let update_display () =
    Mutex.lock state.mutex;
    if state.dirty then (
      let base_doc = if state.document = "" then " " else state.document in
      (* Insert a visual caret at the locally tracked cursor position for
         display purposes only. Do not mutate the authoritative document. *)
      let doc_with_caret = insert_caret_at base_doc state.cursor_pos in
      let aligned_doc = apply_alignment state.alignment doc_with_caret in
      Bogue.Widget.set_text text_display aligned_doc;
      Bogue.Widget.set_text status_label
        (Printf.sprintf "Connected (Cursor: %d)" state.cursor_pos);
      state.dirty <- false);
    Mutex.unlock state.mutex
  in

  (* Polling loop to update GUI safely on main thread *)
  let last_input_text = ref (Bogue.Widget.get_text input_field) in
  let rec poll () =
    let cur = Bogue.Widget.get_text input_field in
    if !last_input_text <> cur then begin
      let l_old = String.length !last_input_text in
      let l_new = String.length cur in
      if l_new > l_old then begin
        (* Ensure server knows our current cursor position before sending
           character insertions so the character is inserted at the intended
           location rather than at the document end. *)
        send_action state (CursorSet state.cursor_pos);
        for i = l_old to l_new - 1 do
          send_action state (KeyPressed cur.[i])
        done;
        (* Clear the input buffer after sending so future typing starts fresh
           and we don't resend prior characters. *)
        Bogue.Widget.set_text input_field "";
        last_input_text := Bogue.Widget.get_text input_field
      end
      else if l_new < l_old then begin
        (* Likewise, send SET before Backspace so server deletes at the right
           position. *)
        send_action state (CursorSet state.cursor_pos);
        for _ = 1 to l_old - l_new do
          send_action state Backspace
        done;
        (* Clearing for deletions as well. *)
        Bogue.Widget.set_text input_field "";
        last_input_text := Bogue.Widget.get_text input_field
      end;
      (* If we didn't clear the input buffer above, update our last observed
         text to the current content; otherwise it's already set to "". *)
      if !last_input_text = "" then () else last_input_text := cur
    end;

    update_display ();
    (* Poll every 50ms *)
    try ignore (Bogue.Timeout.add 50 poll) with _ -> ()
  in
  poll ();

  (* Clicking the text display should focus the hidden input buffer so users
     can start typing directly after clicking the displayed text. This uses a
     best-effort call to Bogue's focus API; if the precise API differs, the
     compile will surface an error and I'll iterate. *)
  (* No Widget.focus calls here; input field remains visible and focusable by clicking it. *)

  (* Alignment buttons *)
  let align_left_btn = Bogue.Widget.button "Align Left" in
  let on_align_left _ =
    send_action state (AlignmentChanged "left");
    state.alignment <- Left;
    state.dirty <- true
  in
  Bogue.Widget.on_click ~click:on_align_left align_left_btn;

  let align_center_btn = Bogue.Widget.button "Align Center" in
  let on_align_center _ =
    send_action state (AlignmentChanged "center");
    state.alignment <- Center;
    state.dirty <- true
  in
  Bogue.Widget.on_click ~click:on_align_center align_center_btn;

  let align_right_btn = Bogue.Widget.button "Align Right" in
  let on_align_right _ =
    send_action state (AlignmentChanged "right");
    state.alignment <- Right;
    state.dirty <- true
  in
  Bogue.Widget.on_click ~click:on_align_right align_right_btn;

  let default_theme_btn = Bogue.Widget.button "Default" in

  let light_theme_btn = Bogue.Widget.button "Light" in
  let dark_theme_btn = Bogue.Widget.button "Dark" in
  let bright_theme_btn = Bogue.Widget.button "Bright" in

  (* Layout *)
  let layout =
    Bogue.Layout.tower
      [
        Bogue.Layout.flat
          [
            Bogue.Layout.resident ~w:120 ~h:50 default_theme_btn;
            Bogue.Layout.resident ~w:120 ~h:50 light_theme_btn;
            Bogue.Layout.resident ~w:140 ~h:50 status_label;
            Bogue.Layout.resident ~w:120 ~h:50 dark_theme_btn;
            Bogue.Layout.resident ~w:120 ~h:50 bright_theme_btn;
          ];
        Bogue.Layout.resident ~w:600 ~h:300 text_display;
        (* Hidden input buffer: keep in layout with a small height so it's
           focusable but minimally visible. Users can click this thin strip to
           begin typing into the shared document. *)
        Bogue.Layout.resident ~w:600 ~h:10 input_field;
        Bogue.Layout.flat ~hmargin:25
          [
            Bogue.Layout.resident ~w:50 ~h:40 left_button;
            Bogue.Layout.resident ~w:50 ~h:40 right_button;
            Bogue.Layout.resident ~w:150 ~h:40 send_button;
            Bogue.Layout.resident ~w:150 ~h:40 backspace_button;
            Bogue.Layout.resident ~w:150 ~h:40 save_button;
          ];
        Bogue.Layout.flat ~hmargin:75
          [
            Bogue.Layout.resident ~w:150 ~h:40 align_left_btn;
            Bogue.Layout.resident ~w:150 ~h:40 align_center_btn;
            Bogue.Layout.resident ~w:150 ~h:40 align_right_btn;
          ];
      ]
  in

  let change_theme theme _ =
    Bogue.Layout.set_background layout
      (Some (Bogue.Layout.color_bg theme.background));
    Bogue.Draw.set_text_color theme.text;
    (* Ensure the UI is refreshed immediately so text color changes take effect
       for existing widgets. Mark state dirty and update display. *)
    (try
       Mutex.lock state.mutex;
       state.dirty <- true;
       Mutex.unlock state.mutex
     with _ -> ());
    (* update_display is in scope; call it to redraw now on the main thread *)
    try update_display () with _ -> ()
  in

  Bogue.Widget.on_click ~click:(change_theme default_theme) default_theme_btn;
  Bogue.Widget.on_click ~click:(change_theme light_theme) light_theme_btn;
  Bogue.Widget.on_click ~click:(change_theme dark_theme) dark_theme_btn;
  Bogue.Widget.on_click ~click:(change_theme bright_theme) bright_theme_btn;

  (* Start background thread to receive updates *)
  let _ = Thread.create (fun () -> receive_updates state) () in
  (* Handle SDL events at board level so we can capture text input globally. We
     forward TEXTINPUT events as KeyPressed characters, and also handle KEYDOWN
     for Backspace and arrow keys as a fallback. *)
  let on_user_event (ev : Tsdl.Sdl.event) : unit =
    try
      let evtyp = Tsdl.Sdl.Event.get ev Tsdl.Sdl.Event.typ in
      if evtyp = Tsdl.Sdl.Event.key_down then
        let keycode = Tsdl.Sdl.Event.get ev Tsdl.Sdl.Event.keyboard_keycode in
        if keycode = Tsdl.Sdl.K.backspace then send_action state Backspace
        else if keycode = Tsdl.Sdl.K.left then (
          let new_pos = max 0 (state.cursor_pos - 1) in
          state.cursor_pos <- new_pos;
          send_action state (CursorSet new_pos))
        else if keycode = Tsdl.Sdl.K.right then (
          let len = String.length state.document in
          let new_pos = min len (state.cursor_pos + 1) in
          state.cursor_pos <- new_pos;
          send_action state (CursorSet new_pos))
        else ()
    with _ -> ()
  in

  (* Run the GUI *)
  let board = Bogue.Bogue.of_layout ~on_user_event layout in
  (* Register global shortcuts so arrow keys and backspace work even when the
     input field is not focused. The actions capture [state] and forward the
     appropriate user actions to the server. *)
  (try
     Bogue.Bogue.add_shortcut Sdl.K.left (fun _ ->
         let new_pos = max 0 (state.cursor_pos - 1) in
         state.cursor_pos <- new_pos;
         send_action state (CursorSet new_pos));
     Bogue.Bogue.add_shortcut Sdl.K.right (fun _ ->
         let len = String.length state.document in
         let new_pos = min len (state.cursor_pos + 1) in
         state.cursor_pos <- new_pos;
         send_action state (CursorSet new_pos));
     Bogue.Bogue.add_shortcut Sdl.K.backspace (fun _ ->
         send_action state Backspace)
   with _ -> ());
  Bogue.Bogue.run board
