open OUnit2
open Backend.Document
open Shared.Events

(* Helper Functions *)

let make_cursor id pos = { id; pos }
let make_cursors pairs = List.map (fun (id, pos) -> make_cursor id pos) pairs

(* Document Creation Tests *)

let test_create_empty _ =
  let doc = create () in
  assert_equal "" (to_string doc) ~printer:(fun s -> s);
  assert_equal 0 (length doc) ~printer:string_of_int

(* Insert Character Tests *)

let test_insert_char_empty _ =
  let doc = "" in
  let result = insert_char doc 0 'a' in
  assert_equal "a" result ~printer:(fun s -> s)

let test_insert_char_beginning _ =
  let doc = "hello" in
  let result = insert_char doc 0 'X' in
  assert_equal "Xhello" result ~printer:(fun s -> s)

let test_insert_char_middle _ =
  let doc = "hello" in
  let result = insert_char doc 2 'X' in
  assert_equal "heXllo" result ~printer:(fun s -> s)

let test_insert_char_end _ =
  let doc = "hello" in
  let result = insert_char doc 5 'X' in
  assert_equal "helloX" result ~printer:(fun s -> s)

let test_insert_char_beyond_bounds _ =
  let doc = "hello" in
  let result = insert_char doc 100 'X' in
  assert_equal "helloX" result ~msg:"Should clamp to end" ~printer:(fun s -> s)

let test_insert_char_negative _ =
  let doc = "hello" in
  let result = insert_char doc (-5) 'X' in
  assert_equal "Xhello" result ~msg:"Should clamp to 0" ~printer:(fun s -> s)

let test_insert_multiple_chars _ =
  let doc = "" in
  let doc = insert_char doc 0 'a' in
  let doc = insert_char doc 1 'b' in
  let doc = insert_char doc 2 'c' in
  assert_equal "abc" doc ~printer:(fun s -> s)

(* Delete Character Tests *)

let test_delete_at_beginning _ =
  let doc = "hello" in
  let result, idx = delete_char_before doc 0 in
  assert_equal "hello" result ~msg:"No change at beginning" ~printer:(fun s ->
      s);
  assert_equal None idx ~msg:"Should return None"

let test_delete_empty_doc _ =
  let doc = "" in
  let result, idx = delete_char_before doc 0 in
  assert_equal "" result ~printer:(fun s -> s);
  assert_equal None idx

let test_delete_single_char _ =
  let doc = "a" in
  let result, idx = delete_char_before doc 1 in
  assert_equal "" result ~printer:(fun s -> s);
  assert_equal (Some 0) idx

let test_delete_middle _ =
  let doc = "hello" in
  let result, idx = delete_char_before doc 3 in
  assert_equal "helo" result ~msg:"Should delete 'l' at index 2"
    ~printer:(fun s -> s);
  assert_equal (Some 2) idx

let test_delete_end _ =
  let doc = "hello" in
  let result, idx = delete_char_before doc 5 in
  assert_equal "hell" result ~msg:"Should delete 'o'" ~printer:(fun s -> s);
  assert_equal (Some 4) idx

let test_delete_beyond_bounds _ =
  let doc = "hello" in
  let result, idx = delete_char_before doc 100 in
  assert_equal "hell" result ~msg:"Should clamp and delete last char"
    ~printer:(fun s -> s);
  assert_equal (Some 4) idx

let test_delete_multiple _ =
  let doc = "hello" in
  let doc, _ = delete_char_before doc 5 in
  let doc, _ = delete_char_before doc 4 in
  let doc, _ = delete_char_before doc 3 in
  assert_equal "he" doc ~printer:(fun s -> s)

(* Apply Local Change - KeyPressed Tests *)

let test_apply_keypressed_empty_doc _ =
  let doc = "" in
  let cursor = make_cursor 1 0 in
  let new_doc, update = apply_local_change doc (KeyPressed 'a') 1 [ cursor ] in
  assert_equal "a" new_doc ~printer:(fun s -> s);
  assert_equal 1 cursor.pos ~msg:"Cursor should advance" ~printer:string_of_int;
  match update with
  | Some (DocumentChanged d) -> assert_equal "a" d
  | _ -> assert_failure "Expected DocumentChanged"

let test_apply_keypressed_single_user _ =
  let doc = "hello" in
  let cursor = make_cursor 1 5 in
  let new_doc, update = apply_local_change doc (KeyPressed '!') 1 [ cursor ] in
  assert_equal "hello!" new_doc ~printer:(fun s -> s);
  assert_equal 6 cursor.pos ~printer:string_of_int;
  match update with
  | Some (DocumentChanged d) -> assert_equal "hello!" d
  | _ -> assert_failure "Expected DocumentChanged"

let test_apply_keypressed_middle _ =
  let doc = "hllo" in
  let cursor = make_cursor 1 1 in
  let new_doc, _ = apply_local_change doc (KeyPressed 'e') 1 [ cursor ] in
  assert_equal "hello" new_doc ~printer:(fun s -> s);
  assert_equal 2 cursor.pos ~printer:string_of_int

let test_apply_keypressed_two_users_same_pos _ =
  let doc = "test" in
  let c1 = make_cursor 1 2 in
  let c2 = make_cursor 2 2 in
  let new_doc, _ = apply_local_change doc (KeyPressed 'X') 1 [ c1; c2 ] in
  assert_equal "teXst" new_doc ~printer:(fun s -> s);
  assert_equal 3 c1.pos ~msg:"Actor should advance" ~printer:string_of_int;
  assert_equal 3 c2.pos ~msg:"Other user at same pos should shift"
    ~printer:string_of_int

let test_apply_keypressed_two_users_before_after _ =
  let doc = "abcd" in
  let c1 = make_cursor 1 1 in
  (* before insertion *)
  let c2 = make_cursor 2 2 in
  (* at insertion point *)
  let c3 = make_cursor 3 3 in
  (* after insertion *)
  let new_doc, _ = apply_local_change doc (KeyPressed 'X') 2 [ c1; c2; c3 ] in
  assert_equal "abXcd" new_doc ~printer:(fun s -> s);
  assert_equal 1 c1.pos ~msg:"User before should stay" ~printer:string_of_int;
  assert_equal 3 c2.pos ~msg:"Actor should advance" ~printer:string_of_int;
  assert_equal 4 c3.pos ~msg:"User after should shift" ~printer:string_of_int

let test_apply_keypressed_multiple_users _ =
  let doc = "hello" in
  let cursors = make_cursors [ (1, 0); (2, 2); (3, 5) ] in
  let new_doc, _ = apply_local_change doc (KeyPressed 'X') 2 cursors in
  assert_equal "heXllo" new_doc ~printer:(fun s -> s);
  assert_equal 0 (List.nth cursors 0).pos ~msg:"User 1 before"
    ~printer:string_of_int;
  assert_equal 3 (List.nth cursors 1).pos ~msg:"User 2 actor"
    ~printer:string_of_int;
  assert_equal 6 (List.nth cursors 2).pos ~msg:"User 3 after"
    ~printer:string_of_int

let test_apply_keypressed_nonexistent_user _ =
  let doc = "hello" in
  let cursor = make_cursor 1 0 in
  let new_doc, update =
    apply_local_change doc (KeyPressed 'X') 999 [ cursor ]
  in
  assert_equal "hello" new_doc ~msg:"Doc should not change" ~printer:(fun s ->
      s);
  assert_equal None update ~msg:"Should return None"

(* ===== Apply Local Change - Backspace Tests ===== *)

let test_apply_backspace_empty_doc _ =
  let doc = "" in
  let cursor = make_cursor 1 0 in
  let new_doc, update = apply_local_change doc Backspace 1 [ cursor ] in
  assert_equal "" new_doc ~printer:(fun s -> s);
  assert_equal 0 cursor.pos ~printer:string_of_int;
  assert_equal None update ~msg:"No change on empty doc"

let test_apply_backspace_at_start _ =
  let doc = "hello" in
  let cursor = make_cursor 1 0 in
  let new_doc, update = apply_local_change doc Backspace 1 [ cursor ] in
  assert_equal "hello" new_doc ~printer:(fun s -> s);
  assert_equal 0 cursor.pos ~printer:string_of_int;
  assert_equal None update

let test_apply_backspace_single_user _ =
  let doc = "hello" in
  let cursor = make_cursor 1 5 in
  let new_doc, update = apply_local_change doc Backspace 1 [ cursor ] in
  assert_equal "hell" new_doc ~printer:(fun s -> s);
  assert_equal 4 cursor.pos ~printer:string_of_int;
  match update with
  | Some (DocumentChanged d) -> assert_equal "hell" d
  | _ -> assert_failure "Expected DocumentChanged"

let test_apply_backspace_middle _ =
  let doc = "hello" in
  let cursor = make_cursor 1 3 in
  let new_doc, _ = apply_local_change doc Backspace 1 [ cursor ] in
  assert_equal "helo" new_doc ~printer:(fun s -> s);
  assert_equal 2 cursor.pos ~printer:string_of_int

let test_apply_backspace_two_users_shift _ =
  let doc = "abc" in
  let c1 = make_cursor 1 1 in
  (* will delete 'a' *)
  let c2 = make_cursor 2 3 in
  (* after deletion point *)
  let new_doc, _ = apply_local_change doc Backspace 1 [ c1; c2 ] in
  assert_equal "bc" new_doc ~printer:(fun s -> s);
  assert_equal 0 c1.pos ~msg:"Actor moves back" ~printer:string_of_int;
  assert_equal 2 c2.pos ~msg:"User after should shift left"
    ~printer:string_of_int

let test_apply_backspace_user_before_deletion _ =
  let doc = "abc" in
  let c1 = make_cursor 1 0 in
  (* before deletion *)
  let c2 = make_cursor 2 2 in
  (* will delete 'b' *)
  let new_doc, _ = apply_local_change doc Backspace 2 [ c1; c2 ] in
  assert_equal "ac" new_doc ~printer:(fun s -> s);
  assert_equal 0 c1.pos ~msg:"User before should stay" ~printer:string_of_int;
  assert_equal 1 c2.pos ~msg:"Actor moves back" ~printer:string_of_int

let test_apply_backspace_multiple_users _ =
  let doc = "hello" in
  let cursors = make_cursors [ (1, 1); (2, 3); (3, 5) ] in
  let new_doc, _ = apply_local_change doc Backspace 2 cursors in
  assert_equal "helo" new_doc ~msg:"Delete 'l' at index 2" ~printer:(fun s -> s);
  assert_equal 1 (List.nth cursors 0).pos ~msg:"User 1 before"
    ~printer:string_of_int;
  assert_equal 2 (List.nth cursors 1).pos ~msg:"User 2 actor"
    ~printer:string_of_int;
  assert_equal 4 (List.nth cursors 2).pos ~msg:"User 3 after"
    ~printer:string_of_int

let test_apply_backspace_user_at_deletion _ =
  let doc = "abc" in
  let c1 = make_cursor 1 1 in
  (* at deletion point *)
  let c2 = make_cursor 2 2 in
  (* will delete 'b' *)
  let new_doc, _ = apply_local_change doc Backspace 2 [ c1; c2 ] in
  assert_equal "ac" new_doc ~printer:(fun s -> s);
  assert_equal 1 c1.pos ~msg:"User at deletion should stay"
    ~printer:string_of_int;
  assert_equal 1 c2.pos ~msg:"Actor moves back" ~printer:string_of_int

(* ===== Apply Local Change - CursorSet Tests ===== *)

let test_apply_cursorset_valid _ =
  let doc = "hello" in
  let cursor = make_cursor 1 0 in
  let new_doc, update = apply_local_change doc (CursorSet 3) 1 [ cursor ] in
  assert_equal "hello" new_doc ~msg:"Doc unchanged" ~printer:(fun s -> s);
  assert_equal 3 cursor.pos ~printer:string_of_int;
  assert_equal None update

let test_apply_cursorset_beyond_doc _ =
  let doc = "hello" in
  let cursor = make_cursor 1 0 in
  let new_doc, _ = apply_local_change doc (CursorSet 100) 1 [ cursor ] in
  assert_equal "hello" new_doc ~printer:(fun s -> s);
  assert_equal 5 cursor.pos ~msg:"Should clamp to doc length"
    ~printer:string_of_int

let test_apply_cursorset_negative _ =
  let doc = "hello" in
  let cursor = make_cursor 1 5 in
  let new_doc, _ = apply_local_change doc (CursorSet (-5)) 1 [ cursor ] in
  assert_equal "hello" new_doc ~printer:(fun s -> s);
  assert_equal 0 cursor.pos ~msg:"Should clamp to 0" ~printer:string_of_int

let test_apply_cursorset_no_affect_others _ =
  let doc = "hello" in
  let c1 = make_cursor 1 0 in
  let c2 = make_cursor 2 5 in
  let new_doc, _ = apply_local_change doc (CursorSet 2) 1 [ c1; c2 ] in
  assert_equal "hello" new_doc ~printer:(fun s -> s);
  assert_equal 2 c1.pos ~msg:"Actor cursor should move" ~printer:string_of_int;
  assert_equal 5 c2.pos ~msg:"Other cursor unchanged" ~printer:string_of_int

(* ===== Apply Local Change - SaveRequest Tests ===== *)

let test_apply_saverequest _ =
  let doc = "hello" in
  let cursor = make_cursor 1 0 in
  let new_doc, update = apply_local_change doc SaveRequest 1 [ cursor ] in
  assert_equal "hello" new_doc ~msg:"Doc unchanged" ~printer:(fun s -> s);
  assert_equal 0 cursor.pos ~msg:"Cursor unchanged" ~printer:string_of_int;
  assert_equal None update

(* ===== Apply Local Change - AlignmentChanged Tests ===== *)

let test_apply_alignment_changed _ =
  let doc = "hello" in
  let cursor = make_cursor 1 0 in
  let new_doc, update =
    apply_local_change doc (AlignmentChanged "center") 1 [ cursor ]
  in
  assert_equal "hello" new_doc ~msg:"Doc unchanged" ~printer:(fun s -> s);
  assert_equal 0 cursor.pos ~msg:"Cursor unchanged" ~printer:string_of_int;
  assert_equal None update

(* ===== Edge Cases and Stress Tests ===== *)

let test_rapid_insertions _ =
  let doc = "" in
  let cursor = make_cursor 1 0 in
  let rec insert_n n doc cursor =
    if n = 0 then doc
    else
      let new_doc, _ = apply_local_change doc (KeyPressed 'a') 1 [ cursor ] in
      insert_n (n - 1) new_doc cursor
  in
  let result = insert_n 100 doc cursor in
  assert_equal 100 (String.length result) ~printer:string_of_int;
  assert_equal 100 cursor.pos ~printer:string_of_int

let test_rapid_deletions _ =
  let doc = String.make 50 'x' in
  let cursor = make_cursor 1 50 in
  let rec delete_n n doc cursor =
    if n = 0 then doc
    else
      let new_doc, update = apply_local_change doc Backspace 1 [ cursor ] in
      match update with
      | None -> doc (* Can't delete anymore *)
      | Some _ -> delete_n (n - 1) new_doc cursor
  in
  let result = delete_n 50 doc cursor in
  assert_equal "" result ~printer:(fun s -> s);
  assert_equal 0 cursor.pos ~printer:string_of_int

let test_interleaved_operations _ =
  let doc = "test" in
  let cursor = make_cursor 1 4 in
  let doc, _ = apply_local_change doc (KeyPressed '!') 1 [ cursor ] in
  let doc, _ = apply_local_change doc Backspace 1 [ cursor ] in
  let doc, _ = apply_local_change doc (KeyPressed '?') 1 [ cursor ] in
  assert_equal "test?" doc ~printer:(fun s -> s);
  assert_equal 5 cursor.pos ~printer:string_of_int

let test_unicode_chars _ =
  let doc = "" in
  let cursor = make_cursor 1 0 in
  (* Test with various ASCII characters to avoid encoding issues *)
  let doc, _ = apply_local_change doc (KeyPressed '@') 1 [ cursor ] in
  let doc, _ = apply_local_change doc (KeyPressed '#') 1 [ cursor ] in
  assert_equal "@#" doc ~printer:(fun s -> s)

let test_special_chars _ =
  let doc = "" in
  let cursor = make_cursor 1 0 in
  let doc, _ = apply_local_change doc (KeyPressed '\n') 1 [ cursor ] in
  let doc, _ = apply_local_change doc (KeyPressed '\t') 1 [ cursor ] in
  let doc, _ = apply_local_change doc (KeyPressed ' ') 1 [ cursor ] in
  assert_equal 3 (String.length doc) ~printer:string_of_int

(* ===== Complex Multi-User Scenarios ===== *)

let test_three_users_concurrent_edits _ =
  let doc = "abcdef" in
  let cursors = make_cursors [ (1, 2); (2, 4); (3, 6) ] in
  (* User 1 types at pos 2 *)
  let doc, _ = apply_local_change doc (KeyPressed 'X') 1 cursors in
  assert_equal "abXcdef" doc ~printer:(fun s -> s);
  assert_equal 3 (List.nth cursors 0).pos;
  assert_equal 5 (List.nth cursors 1).pos;
  assert_equal 7 (List.nth cursors 2).pos;
  (* User 2 deletes at pos 5 (deletes 'd' at index 4) *)
  let doc, _ = apply_local_change doc Backspace 2 cursors in
  assert_equal "abXcef" doc ~printer:(fun s -> s);
  assert_equal 3 (List.nth cursors 0).pos;
  assert_equal 4 (List.nth cursors 1).pos;
  assert_equal 6 (List.nth cursors 2).pos

let test_five_users_same_position _ =
  let doc = "test" in
  let cursors = make_cursors [ (1, 2); (2, 2); (3, 2); (4, 2); (5, 2) ] in
  let doc, _ = apply_local_change doc (KeyPressed 'X') 3 cursors in
  assert_equal "teXst" doc ~printer:(fun s -> s);
  List.iteri
    (fun i c ->
      let expected = if c.id = 3 then 3 else 3 in
      assert_equal expected c.pos ~msg:(Printf.sprintf "User %d" (i + 1)))
    cursors

(* ===== Test Suite ===== *)

let suite =
  "Document Module Tests"
  >::: [
         (* Document creation *)
         "test_create_empty" >:: test_create_empty;
         (* Insert char tests *)
         "test_insert_char_empty" >:: test_insert_char_empty;
         "test_insert_char_beginning" >:: test_insert_char_beginning;
         "test_insert_char_middle" >:: test_insert_char_middle;
         "test_insert_char_end" >:: test_insert_char_end;
         "test_insert_char_beyond_bounds" >:: test_insert_char_beyond_bounds;
         "test_insert_char_negative" >:: test_insert_char_negative;
         "test_insert_multiple_chars" >:: test_insert_multiple_chars;
         (* Delete char tests *)
         "test_delete_at_beginning" >:: test_delete_at_beginning;
         "test_delete_empty_doc" >:: test_delete_empty_doc;
         "test_delete_single_char" >:: test_delete_single_char;
         "test_delete_middle" >:: test_delete_middle;
         "test_delete_end" >:: test_delete_end;
         "test_delete_beyond_bounds" >:: test_delete_beyond_bounds;
         "test_delete_multiple" >:: test_delete_multiple;
         (* Apply KeyPressed tests *)
         "test_apply_keypressed_empty_doc" >:: test_apply_keypressed_empty_doc;
         "test_apply_keypressed_single_user"
         >:: test_apply_keypressed_single_user;
         "test_apply_keypressed_middle" >:: test_apply_keypressed_middle;
         "test_apply_keypressed_two_users_same_pos"
         >:: test_apply_keypressed_two_users_same_pos;
         "test_apply_keypressed_two_users_before_after"
         >:: test_apply_keypressed_two_users_before_after;
         "test_apply_keypressed_multiple_users"
         >:: test_apply_keypressed_multiple_users;
         "test_apply_keypressed_nonexistent_user"
         >:: test_apply_keypressed_nonexistent_user;
         (* Apply Backspace tests *)
         "test_apply_backspace_empty_doc" >:: test_apply_backspace_empty_doc;
         "test_apply_backspace_at_start" >:: test_apply_backspace_at_start;
         "test_apply_backspace_single_user" >:: test_apply_backspace_single_user;
         "test_apply_backspace_middle" >:: test_apply_backspace_middle;
         "test_apply_backspace_two_users_shift"
         >:: test_apply_backspace_two_users_shift;
         "test_apply_backspace_user_before_deletion"
         >:: test_apply_backspace_user_before_deletion;
         "test_apply_backspace_multiple_users"
         >:: test_apply_backspace_multiple_users;
         "test_apply_backspace_user_at_deletion"
         >:: test_apply_backspace_user_at_deletion;
         (* Apply CursorSet tests *)
         "test_apply_cursorset_valid" >:: test_apply_cursorset_valid;
         "test_apply_cursorset_beyond_doc" >:: test_apply_cursorset_beyond_doc;
         "test_apply_cursorset_negative" >:: test_apply_cursorset_negative;
         "test_apply_cursorset_no_affect_others"
         >:: test_apply_cursorset_no_affect_others;
         (* Apply SaveRequest tests *)
         "test_apply_saverequest" >:: test_apply_saverequest;
         (* Apply AlignmentChanged tests *)
         "test_apply_alignment_changed" >:: test_apply_alignment_changed;
         (* Edge cases and stress tests *)
         "test_rapid_insertions" >:: test_rapid_insertions;
         "test_rapid_deletions" >:: test_rapid_deletions;
         "test_interleaved_operations" >:: test_interleaved_operations;
         "test_unicode_chars" >:: test_unicode_chars;
         "test_special_chars" >:: test_special_chars;
         (* Complex multi-user scenarios *)
         "test_three_users_concurrent_edits"
         >:: test_three_users_concurrent_edits;
         "test_five_users_same_position" >:: test_five_users_same_position;
       ]

let () = run_test_tt_main suite
