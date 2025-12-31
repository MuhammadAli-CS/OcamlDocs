open OUnit2
open Backend.Server
open Shared.Events

(* Wire Protocol Tests *)

(* Test string_of_user_action *)

let test_string_of_keypressed _ =
  let action = KeyPressed 'a' in
  let result = string_of_user_action action in
  assert_equal "KEY 97" result ~printer:(fun s -> s)

let test_string_of_backspace _ =
  let action = Backspace in
  let result = string_of_user_action action in
  assert_equal "BACKSPACE" result ~printer:(fun s -> s)

let test_string_of_cursorset _ =
  let action = CursorSet 42 in
  let result = string_of_user_action action in
  assert_equal "SET 42" result ~printer:(fun s -> s)

let test_string_of_saverequest _ =
  let action = SaveRequest in
  let result = string_of_user_action action in
  assert_equal "SAVE" result ~printer:(fun s -> s)

let test_string_of_alignment _ =
  let action = AlignmentChanged "center" in
  let result = string_of_user_action action in
  assert_equal "ALIGN center" result ~printer:(fun s -> s)

let test_string_of_special_chars _ =
  assert_equal "KEY 32" (string_of_user_action (KeyPressed ' ')) ~msg:"space";
  assert_equal "KEY 10" (string_of_user_action (KeyPressed '\n')) ~msg:"newline";
  assert_equal "KEY 9" (string_of_user_action (KeyPressed '\t')) ~msg:"tab"

(* Test user_action_of_string *)

let test_parse_keypressed_valid _ =
  match user_action_of_string "KEY 97" with
  | Some (KeyPressed c) -> assert_equal 'a' c ~printer:(String.make 1)
  | _ -> assert_failure "Expected KeyPressed 'a'"

let test_parse_backspace _ =
  match user_action_of_string "BACKSPACE" with
  | Some Backspace -> assert_bool "Success" true
  | _ -> assert_failure "Expected Backspace"

let test_parse_cursorset_valid _ =
  match user_action_of_string "SET 42" with
  | Some (CursorSet pos) -> assert_equal 42 pos ~printer:string_of_int
  | _ -> assert_failure "Expected CursorSet 42"

let test_parse_save _ =
  match user_action_of_string "SAVE" with
  | Some SaveRequest -> assert_bool "Success" true
  | _ -> assert_failure "Expected SaveRequest"

let test_parse_alignment _ =
  match user_action_of_string "ALIGN left" with
  | Some (AlignmentChanged align) ->
      assert_equal "left" align ~printer:(fun s -> s)
  | _ -> assert_failure "Expected AlignmentChanged 'left'"

let test_parse_alignment_center _ =
  match user_action_of_string "ALIGN center" with
  | Some (AlignmentChanged align) ->
      assert_equal "center" align ~printer:(fun s -> s)
  | _ -> assert_failure "Expected AlignmentChanged 'center'"

let test_parse_alignment_right _ =
  match user_action_of_string "ALIGN right" with
  | Some (AlignmentChanged align) ->
      assert_equal "right" align ~printer:(fun s -> s)
  | _ -> assert_failure "Expected AlignmentChanged 'right'"

let test_parse_invalid_key_nonumber _ =
  match user_action_of_string "KEY abc" with
  | None -> assert_bool "Success" true
  | _ -> assert_failure "Expected None for invalid key"

let test_parse_invalid_key_outofbounds _ =
  match user_action_of_string "KEY 256" with
  | None -> assert_bool "Success" true
  | _ -> assert_failure "Expected None for out of bounds key"

let test_parse_invalid_key_negative _ =
  match user_action_of_string "KEY -1" with
  | None -> assert_bool "Success" true
  | _ -> assert_failure "Expected None for negative key"

let test_parse_invalid_set_nonumber _ =
  match user_action_of_string "SET abc" with
  | None -> assert_bool "Success" true
  | _ -> assert_failure "Expected None for invalid set"

let test_parse_unknown_command _ =
  match user_action_of_string "UNKNOWN" with
  | None -> assert_bool "Success" true
  | _ -> assert_failure "Expected None for unknown command"

let test_parse_empty_string _ =
  match user_action_of_string "" with
  | None -> assert_bool "Success" true
  | _ -> assert_failure "Expected None for empty string"

let test_parse_whitespace_only _ =
  match user_action_of_string "   " with
  | None -> assert_bool "Success" true
  | _ -> assert_failure "Expected None for whitespace"

let test_parse_with_leading_whitespace _ =
  match user_action_of_string "  BACKSPACE  " with
  | Some Backspace -> assert_bool "Success" true
  | _ -> assert_failure "Should handle whitespace"

let test_parse_with_extra_args _ =
  match user_action_of_string "BACKSPACE extra args" with
  | None -> assert_bool "Success" true
  | _ -> assert_failure "Should reject extra arguments"

(* Test string_of_backend_update *)

let test_string_of_full_document _ =
  let update = FullDocument "hello world" in
  let result = string_of_backend_update update in
  assert_equal "FULL hello world" result ~printer:(fun s -> s)

let test_string_of_document_changed _ =
  let update = DocumentChanged "new text" in
  let result = string_of_backend_update update in
  assert_equal "DOC new text" result ~printer:(fun s -> s)

let test_string_of_cursor_moved _ =
  let update = CursorMoved 15 in
  let result = string_of_backend_update update in
  assert_equal "CURSOR 15" result ~printer:(fun s -> s)

let test_string_of_alignment_update _ =
  let update = AlignmentUpdate "center" in
  let result = string_of_backend_update update in
  assert_equal "ALIGN center" result ~printer:(fun s -> s)

let test_string_of_empty_document _ =
  let update = FullDocument "" in
  let result = string_of_backend_update update in
  assert_equal "FULL " result ~printer:(fun s -> s)

let test_string_of_document_with_spaces _ =
  let update = DocumentChanged "hello   world" in
  let result = string_of_backend_update update in
  assert_equal "DOC hello   world" result ~printer:(fun s -> s)

(* ===== Round-trip Tests ===== *)

let test_roundtrip_keypressed _ =
  let original = KeyPressed 'x' in
  let encoded = string_of_user_action original in
  match user_action_of_string encoded with
  | Some (KeyPressed c) -> assert_equal 'x' c ~printer:(String.make 1)
  | _ -> assert_failure "Round-trip failed"

let test_roundtrip_backspace _ =
  let original = Backspace in
  let encoded = string_of_user_action original in
  match user_action_of_string encoded with
  | Some Backspace -> assert_bool "Success" true
  | _ -> assert_failure "Round-trip failed"

let test_roundtrip_cursorset _ =
  let original = CursorSet 123 in
  let encoded = string_of_user_action original in
  match user_action_of_string encoded with
  | Some (CursorSet pos) -> assert_equal 123 pos ~printer:string_of_int
  | _ -> assert_failure "Round-trip failed"

let test_roundtrip_save _ =
  let original = SaveRequest in
  let encoded = string_of_user_action original in
  match user_action_of_string encoded with
  | Some SaveRequest -> assert_bool "Success" true
  | _ -> assert_failure "Round-trip failed"

let test_roundtrip_alignment _ =
  let original = AlignmentChanged "right" in
  let encoded = string_of_user_action original in
  match user_action_of_string encoded with
  | Some (AlignmentChanged align) ->
      assert_equal "right" align ~printer:(fun s -> s)
  | _ -> assert_failure "Round-trip failed"

let test_roundtrip_all_ascii _ =
  for code = 0 to 255 do
    let original = KeyPressed (Char.chr code) in
    let encoded = string_of_user_action original in
    match user_action_of_string encoded with
    | Some (KeyPressed c) ->
        assert_equal code (Char.code c)
          ~msg:(Printf.sprintf "Failed for char code %d" code)
          ~printer:string_of_int
    | _ ->
        assert_failure
          (Printf.sprintf "Round-trip failed for char code %d" code)
  done

(* Edge Cases *)

let test_parse_set_zero _ =
  match user_action_of_string "SET 0" with
  | Some (CursorSet pos) -> assert_equal 0 pos ~printer:string_of_int
  | _ -> assert_failure "Expected CursorSet 0"

let test_parse_set_large_number _ =
  match user_action_of_string "SET 999999" with
  | Some (CursorSet pos) -> assert_equal 999999 pos ~printer:string_of_int
  | _ -> assert_failure "Expected CursorSet 999999"

let test_parse_key_zero _ =
  match user_action_of_string "KEY 0" with
  | Some (KeyPressed c) -> assert_equal '\000' c
  | _ -> assert_failure "Expected KeyPressed '\\000'"

let test_parse_key_255 _ =
  match user_action_of_string "KEY 255" with
  | Some (KeyPressed c) -> assert_equal 255 (Char.code c) ~printer:string_of_int
  | _ -> assert_failure "Expected KeyPressed char 255"

let test_parse_case_sensitive _ =
  assert_equal None
    (user_action_of_string "backspace")
    ~msg:"Should be case-sensitive";
  assert_equal None
    (user_action_of_string "Backspace")
    ~msg:"Should be case-sensitive";
  assert_equal None
    (user_action_of_string "key 97")
    ~msg:"Should be case-sensitive"

let test_decode_full_document _ =
  match _backend_update_of_string "FULL test document" with
  | Some (FullDocument doc) ->
      assert_equal "test document" doc ~printer:(fun s -> s)
  | _ -> assert_failure "Expected FullDocument"

let test_decode_document_changed _ =
  match _backend_update_of_string "DOC updated text" with
  | Some (DocumentChanged doc) ->
      assert_equal "updated text" doc ~printer:(fun s -> s)
  | _ -> assert_failure "Expected DocumentChanged"

let test_decode_short_full _ =
  match _backend_update_of_string "FULL" with
  | None -> assert_bool "Should return None for incomplete message" true
  | _ -> assert_failure "Expected None"

let test_decode_short_doc _ =
  match _backend_update_of_string "DOC" with
  | None -> assert_bool "Should return None for incomplete message" true
  | _ -> assert_failure "Expected None"

let test_decode_invalid _ =
  match _backend_update_of_string "INVALID" with
  | None -> assert_bool "Success" true
  | _ -> assert_failure "Expected None"

(* Test Suite *)

let suite =
  "Server Protocol Tests"
  >::: [
         (* Encoding user actions *)
         "test_string_of_keypressed" >:: test_string_of_keypressed;
         "test_string_of_backspace" >:: test_string_of_backspace;
         "test_string_of_cursorset" >:: test_string_of_cursorset;
         "test_string_of_saverequest" >:: test_string_of_saverequest;
         "test_string_of_alignment" >:: test_string_of_alignment;
         "test_string_of_special_chars" >:: test_string_of_special_chars;
         (* Parsing user actions *)
         "test_parse_keypressed_valid" >:: test_parse_keypressed_valid;
         "test_parse_backspace" >:: test_parse_backspace;
         "test_parse_cursorset_valid" >:: test_parse_cursorset_valid;
         "test_parse_save" >:: test_parse_save;
         "test_parse_alignment" >:: test_parse_alignment;
         "test_parse_alignment_center" >:: test_parse_alignment_center;
         "test_parse_alignment_right" >:: test_parse_alignment_right;
         "test_parse_invalid_key_nonumber" >:: test_parse_invalid_key_nonumber;
         "test_parse_invalid_key_outofbounds"
         >:: test_parse_invalid_key_outofbounds;
         "test_parse_invalid_key_negative" >:: test_parse_invalid_key_negative;
         "test_parse_invalid_set_nonumber" >:: test_parse_invalid_set_nonumber;
         "test_parse_unknown_command" >:: test_parse_unknown_command;
         "test_parse_empty_string" >:: test_parse_empty_string;
         "test_parse_whitespace_only" >:: test_parse_whitespace_only;
         "test_parse_with_leading_whitespace"
         >:: test_parse_with_leading_whitespace;
         "test_parse_with_extra_args" >:: test_parse_with_extra_args;
         (* Encoding backend updates *)
         "test_string_of_full_document" >:: test_string_of_full_document;
         "test_string_of_document_changed" >:: test_string_of_document_changed;
         "test_string_of_cursor_moved" >:: test_string_of_cursor_moved;
         "test_string_of_alignment_update" >:: test_string_of_alignment_update;
         "test_string_of_empty_document" >:: test_string_of_empty_document;
         "test_string_of_document_with_spaces"
         >:: test_string_of_document_with_spaces;
         (* Round-trip tests *)
         "test_roundtrip_keypressed" >:: test_roundtrip_keypressed;
         "test_roundtrip_backspace" >:: test_roundtrip_backspace;
         "test_roundtrip_cursorset" >:: test_roundtrip_cursorset;
         "test_roundtrip_save" >:: test_roundtrip_save;
         "test_roundtrip_alignment" >:: test_roundtrip_alignment;
         "test_roundtrip_all_ascii" >:: test_roundtrip_all_ascii;
         (* Edge cases *)
         "test_parse_set_zero" >:: test_parse_set_zero;
         "test_parse_set_large_number" >:: test_parse_set_large_number;
         "test_parse_key_zero" >:: test_parse_key_zero;
         "test_parse_key_255" >:: test_parse_key_255;
         "test_parse_case_sensitive" >:: test_parse_case_sensitive;
         "test_decode_full_document" >:: test_decode_full_document;
         "test_decode_document_changed" >:: test_decode_document_changed;
         "test_decode_short_full" >:: test_decode_short_full;
         "test_decode_short_doc" >:: test_decode_short_doc;
         "test_decode_invalid" >:: test_decode_invalid;
       ]

let () = run_test_tt_main suite
