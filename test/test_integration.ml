open OUnit2
open Backend.Document
open Shared.Events

(* Helper Functions *)

let make_cursor id pos = { id; pos }
let make_cursors pairs = List.map (fun (id, pos) -> make_cursor id pos) pairs

(* Integration Scenario Tests *)

(* Simulate a complete editing session with one user *)
let test_single_user_session _ =
  let doc = create () in
  let cursor = make_cursor 1 0 in

  (* Type "Hello" *)
  let doc, _ = apply_local_change doc (KeyPressed 'H') 1 [ cursor ] in
  let doc, _ = apply_local_change doc (KeyPressed 'e') 1 [ cursor ] in
  let doc, _ = apply_local_change doc (KeyPressed 'l') 1 [ cursor ] in
  let doc, _ = apply_local_change doc (KeyPressed 'l') 1 [ cursor ] in
  let doc, _ = apply_local_change doc (KeyPressed 'o') 1 [ cursor ] in

  assert_equal "Hello" doc ~printer:(fun s -> s);
  assert_equal 5 cursor.pos ~printer:string_of_int;

  (* Add a space and "World" *)
  let doc, _ = apply_local_change doc (KeyPressed ' ') 1 [ cursor ] in
  let doc, _ = apply_local_change doc (KeyPressed 'W') 1 [ cursor ] in
  let doc, _ = apply_local_change doc (KeyPressed 'o') 1 [ cursor ] in
  let doc, _ = apply_local_change doc (KeyPressed 'r') 1 [ cursor ] in
  let doc, _ = apply_local_change doc (KeyPressed 'l') 1 [ cursor ] in
  let doc, _ = apply_local_change doc (KeyPressed 'd') 1 [ cursor ] in

  assert_equal "Hello World" doc ~printer:(fun s -> s);
  assert_equal 11 cursor.pos ~printer:string_of_int;

  (* Delete "World" one char at a time *)
  let doc, _ = apply_local_change doc Backspace 1 [ cursor ] in
  let doc, _ = apply_local_change doc Backspace 1 [ cursor ] in
  let doc, _ = apply_local_change doc Backspace 1 [ cursor ] in
  let doc, _ = apply_local_change doc Backspace 1 [ cursor ] in
  let doc, _ = apply_local_change doc Backspace 1 [ cursor ] in
  let doc, _ = apply_local_change doc Backspace 1 [ cursor ] in

  assert_equal "Hello" doc ~printer:(fun s -> s);
  assert_equal 5 cursor.pos ~printer:string_of_int

(* Two users typing simultaneously in different locations *)
let test_two_users_concurrent_typing _ =
  let doc = "The quick brown fox" in
  let c1 = make_cursor 1 4 in
  (* After "The " *)
  let c2 = make_cursor 2 10 in
  (* After "The quick " *)

  (* User 1 types "very " *)
  let doc, _ = apply_local_change doc (KeyPressed 'v') 1 [ c1; c2 ] in
  assert_equal "The vquick brown fox" doc ~printer:(fun s -> s);
  assert_equal 5 c1.pos;
  assert_equal 11 c2.pos;

  (* Shifted right *)
  let doc, _ = apply_local_change doc (KeyPressed 'e') 1 [ c1; c2 ] in
  let doc, _ = apply_local_change doc (KeyPressed 'r') 1 [ c1; c2 ] in
  let doc, _ = apply_local_change doc (KeyPressed 'y') 1 [ c1; c2 ] in
  let doc, _ = apply_local_change doc (KeyPressed ' ') 1 [ c1; c2 ] in

  assert_equal "The very quick brown fox" doc ~printer:(fun s -> s);
  assert_equal 9 c1.pos;
  assert_equal 15 c2.pos;

  (* User 2 types "red " *)
  let doc, _ = apply_local_change doc (KeyPressed 'r') 2 [ c1; c2 ] in
  let doc, _ = apply_local_change doc (KeyPressed 'e') 2 [ c1; c2 ] in
  let doc, _ = apply_local_change doc (KeyPressed 'd') 2 [ c1; c2 ] in
  let doc, _ = apply_local_change doc (KeyPressed ' ') 2 [ c1; c2 ] in

  assert_equal "The very quick red brown fox" doc ~printer:(fun s -> s);
  assert_equal 9 c1.pos;
  (* Unaffected *)
  assert_equal 19 c2.pos

(* Three users collaborating on a document *)
let test_three_users_collaboration _ =
  let doc = "" in
  let cursors = make_cursors [ (1, 0); (2, 0); (3, 0) ] in

  (* User 1 starts typing *)
  let doc, _ = apply_local_change doc (KeyPressed 'A') 1 cursors in
  let doc, _ = apply_local_change doc (KeyPressed 'B') 1 cursors in
  let doc, _ = apply_local_change doc (KeyPressed 'C') 1 cursors in

  assert_equal "ABC" doc ~printer:(fun s -> s);

  (* User 2 inserts at beginning *)
  let _, _ = apply_local_change doc (CursorSet 0) 2 cursors in
  let doc, _ = apply_local_change doc (KeyPressed '1') 2 cursors in

  assert_equal "1ABC" doc ~printer:(fun s -> s);
  assert_equal 4 (List.nth cursors 0).pos;
  (* User 1 shifted *)
  assert_equal 1 (List.nth cursors 1).pos;

  (* User 2 advanced *)

  (* User 3 inserts in middle *)
  let _, _ = apply_local_change doc (CursorSet 2) 3 cursors in
  let doc, _ = apply_local_change doc (KeyPressed 'X') 3 cursors in

  assert_equal "1AXBC" doc ~printer:(fun s -> s)

(* Simulate a correction scenario: type, delete, retype *)
let test_correction_scenario _ =
  let doc = "" in
  let cursor = make_cursor 1 0 in

  (* Type "teh" (typo) *)
  let doc, _ = apply_local_change doc (KeyPressed 't') 1 [ cursor ] in
  let doc, _ = apply_local_change doc (KeyPressed 'e') 1 [ cursor ] in
  let doc, _ = apply_local_change doc (KeyPressed 'h') 1 [ cursor ] in

  assert_equal "teh" doc ~printer:(fun s -> s);

  (* Realize mistake, backspace 2 chars *)
  let doc, _ = apply_local_change doc Backspace 1 [ cursor ] in
  let doc, _ = apply_local_change doc Backspace 1 [ cursor ] in

  assert_equal "t" doc ~printer:(fun s -> s);
  assert_equal 1 cursor.pos;

  (* Type "he" correctly *)
  let doc, _ = apply_local_change doc (KeyPressed 'h') 1 [ cursor ] in
  let doc, _ = apply_local_change doc (KeyPressed 'e') 1 [ cursor ] in

  assert_equal "the" doc ~printer:(fun s -> s);
  assert_equal 3 cursor.pos

(* Multiple users editing with one user deleting *)
let test_multi_user_with_deletion _ =
  let doc = "ABCDEFGH" in
  let cursors = make_cursors [ (1, 2); (2, 5); (3, 8) ] in

  (* User 2 at position 5 (after 'E') deletes 'E' *)
  let doc, _ = apply_local_change doc Backspace 2 cursors in

  assert_equal "ABCDFGH" doc ~printer:(fun s -> s);
  assert_equal 2 (List.nth cursors 0).pos;
  (* User 1 unaffected *)
  assert_equal 4 (List.nth cursors 1).pos;
  (* User 2 moved back *)
  assert_equal 7 (List.nth cursors 2).pos;

  (* User 3 shifted left *)

  (* User 1 types at position 2 *)
  let doc, _ = apply_local_change doc (KeyPressed 'X') 1 cursors in

  assert_equal "ABXCDFGH" doc ~printer:(fun s -> s);
  assert_equal 3 (List.nth cursors 0).pos;
  (* User 1 advanced *)
  assert_equal 5 (List.nth cursors 1).pos;
  (* User 2 shifted *)
  assert_equal 8 (List.nth cursors 2).pos (* User 3 shifted *)

(* Simulate rapid concurrent edits from multiple users *)
let test_rapid_concurrent_edits _ =
  let doc = "start" in
  let cursors = make_cursors [ (1, 5); (2, 0) ] in

  (* Start: "start" (5 chars), c1 at 5, c2 at 0 *)
  (* User 1 types 'A' at 5: "startA", c1->6, c2->0 *)
  let doc, _ = apply_local_change doc (KeyPressed 'A') 1 cursors in
  (* User 2 types 'Z' at 0: "ZstartA", c1->7, c2->1 *)
  let doc, _ = apply_local_change doc (KeyPressed 'Z') 2 cursors in
  (* User 1 types 'B' at 7: "ZstartAB", c1->8, c2->1 *)
  let doc, _ = apply_local_change doc (KeyPressed 'B') 1 cursors in
  (* User 2 types 'Y' at 1: "ZYstartAB", c1->9, c2->2 *)
  let doc, _ = apply_local_change doc (KeyPressed 'Y') 2 cursors in
  (* User 1 types 'C' at 9: "ZYstartABC" *)
  let doc, _ = apply_local_change doc (KeyPressed 'C') 1 cursors in

  assert_equal "ZYstartABC" doc ~printer:(fun s -> s)

(* Insert and delete at the same position *)
let test_insert_delete_same_position _ =
  let doc = "test" in
  let cursor = make_cursor 1 2 in

  (* Insert at position 2 *)
  let doc, _ = apply_local_change doc (KeyPressed 'X') 1 [ cursor ] in
  assert_equal "teXst" doc ~printer:(fun s -> s);
  assert_equal 3 cursor.pos;

  (* Delete what we just inserted *)
  let doc, _ = apply_local_change doc Backspace 1 [ cursor ] in
  assert_equal "test" doc ~printer:(fun s -> s);
  assert_equal 2 cursor.pos

(* Cursor movements during concurrent edits *)
let test_cursor_movements _ =
  let doc = "0123456789" in
  let cursor = make_cursor 1 5 in

  (* Move to beginning *)
  let _, _ = apply_local_change doc (CursorSet 0) 1 [ cursor ] in
  assert_equal 0 cursor.pos;

  (* Type something *)
  let doc, _ = apply_local_change doc (KeyPressed 'X') 1 [ cursor ] in
  assert_equal "X0123456789" doc ~printer:(fun s -> s);
  assert_equal 1 cursor.pos;

  (* Move to end *)
  let _, _ = apply_local_change doc (CursorSet 11) 1 [ cursor ] in
  assert_equal 11 cursor.pos;

  (* Type at end *)
  let doc, _ = apply_local_change doc (KeyPressed 'Y') 1 [ cursor ] in
  assert_equal "X0123456789Y" doc ~printer:(fun s -> s);
  assert_equal 12 cursor.pos

(* Stress test: Many users, many operations *)
let test_many_users_many_operations _ =
  let doc = "" in
  let num_users = 5 in
  let cursors = make_cursors (List.init num_users (fun i -> (i + 1, 0))) in

  (* Each user types their user ID as a character *)
  let doc =
    List.fold_left
      (fun d cursor ->
        let char = Char.chr (48 + cursor.id) in
        (* '1', '2', ... *)
        let new_doc, _ =
          apply_local_change d (KeyPressed char) cursor.id cursors
        in
        new_doc)
      doc cursors
  in

  (* Document should contain all user IDs *)
  assert_equal 5 (String.length doc) ~printer:string_of_int;
  assert_bool "Contains user IDs" (String.contains doc '1');
  assert_bool "Contains user IDs" (String.contains doc '2');
  assert_bool "Contains user IDs" (String.contains doc '3')

(* Test complete workflow: connect, edit, save *)
let test_complete_workflow _ =
  let doc = create () in
  let cursor = make_cursor 1 0 in

  (* Initial state *)
  assert_equal "" (to_string doc) ~printer:(fun s -> s);
  assert_equal 0 (length doc) ~printer:string_of_int;

  (* Edit session *)
  let doc, _ = apply_local_change doc (KeyPressed 'H') 1 [ cursor ] in
  let doc, _ = apply_local_change doc (KeyPressed 'i') 1 [ cursor ] in

  assert_equal "Hi" doc ~printer:(fun s -> s);

  (* Save request (doesn't change doc) *)
  let doc, update = apply_local_change doc SaveRequest 1 [ cursor ] in
  assert_equal "Hi" doc ~printer:(fun s -> s);
  assert_equal None update;

  (* Alignment change (doesn't change doc) *)
  let doc, update =
    apply_local_change doc (AlignmentChanged "center") 1 [ cursor ]
  in
  assert_equal "Hi" doc ~printer:(fun s -> s);
  assert_equal None update

(* Test boundary case: all users at document end *)
let test_all_users_at_end _ =
  let doc = "test" in
  let cursors = make_cursors [ (1, 4); (2, 4); (3, 4) ] in

  (* User 1 types *)
  let doc, _ = apply_local_change doc (KeyPressed '!') 1 cursors in

  assert_equal "test!" doc ~printer:(fun s -> s);
  assert_equal 5 (List.nth cursors 0).pos;
  (* User 1 advanced *)
  assert_equal 5 (List.nth cursors 1).pos;
  (* User 2 shifted *)
  assert_equal 5 (List.nth cursors 2).pos (* User 3 shifted *)

(* Test boundary case: all users at document start *)
let test_all_users_at_start _ =
  let doc = "test" in
  let cursors = make_cursors [ (1, 0); (2, 0); (3, 0) ] in

  (* User 1 types *)
  let doc, _ = apply_local_change doc (KeyPressed 'X') 1 cursors in

  assert_equal "Xtest" doc ~printer:(fun s -> s);
  assert_equal 1 (List.nth cursors 0).pos;
  (* User 1 advanced *)
  assert_equal 1 (List.nth cursors 1).pos;
  (* User 2 shifted *)
  assert_equal 1 (List.nth cursors 2).pos (* User 3 shifted *)

(* Alternating insertions and deletions *)
let test_alternating_ops _ =
  let doc = "base" in
  let cursor = make_cursor 1 4 in

  let doc, _ = apply_local_change doc (KeyPressed 'X') 1 [ cursor ] in
  let doc, _ = apply_local_change doc Backspace 1 [ cursor ] in
  let doc, _ = apply_local_change doc (KeyPressed 'Y') 1 [ cursor ] in
  let doc, _ = apply_local_change doc Backspace 1 [ cursor ] in
  let doc, _ = apply_local_change doc (KeyPressed 'Z') 1 [ cursor ] in

  assert_equal "baseZ" doc ~printer:(fun s -> s);
  assert_equal 5 cursor.pos

(* Users leapfrogging each other *)
let test_leapfrogging_users _ =
  let doc = "ABCDEFGH" in
  let c1 = make_cursor 1 2 in
  (* After 'B' *)
  let c2 = make_cursor 2 6 in
  (* After 'F' *)

  (* User 1 types, pushing user 2 *)
  let doc, _ = apply_local_change doc (KeyPressed '1') 1 [ c1; c2 ] in
  assert_equal "AB1CDEFGH" doc ~printer:(fun s -> s);
  assert_equal 3 c1.pos;
  assert_equal 7 c2.pos;

  (* User 2 deletes at pos 7 (deletes 'F' at index 6) *)
  let doc, _ = apply_local_change doc Backspace 2 [ c1; c2 ] in
  assert_equal "AB1CDEGH" doc ~printer:(fun s -> s);
  assert_equal 3 c1.pos;
  (* Unchanged *)
  assert_equal 6 c2.pos (* Moved back *)

(* Test with long document and many edits *)
let test_long_document _ =
  let long_doc = String.make 1000 'x' in
  let cursor = make_cursor 1 500 in

  (* Insert in the middle *)
  let doc, _ = apply_local_change long_doc (KeyPressed 'M') 1 [ cursor ] in
  assert_equal 1001 (String.length doc) ~printer:string_of_int;
  assert_equal 501 cursor.pos ~printer:string_of_int;

  (* Delete in the middle *)
  let doc, _ = apply_local_change doc Backspace 1 [ cursor ] in
  assert_equal 1000 (String.length doc) ~printer:string_of_int;
  assert_equal 500 cursor.pos ~printer:string_of_int

(* ===== Test Suite ===== *)

let suite =
  "Integration Tests"
  >::: [
         "test_single_user_session" >:: test_single_user_session;
         "test_two_users_concurrent_typing" >:: test_two_users_concurrent_typing;
         "test_three_users_collaboration" >:: test_three_users_collaboration;
         "test_correction_scenario" >:: test_correction_scenario;
         "test_multi_user_with_deletion" >:: test_multi_user_with_deletion;
         "test_rapid_concurrent_edits" >:: test_rapid_concurrent_edits;
         "test_insert_delete_same_position" >:: test_insert_delete_same_position;
         "test_cursor_movements" >:: test_cursor_movements;
         "test_many_users_many_operations" >:: test_many_users_many_operations;
         "test_complete_workflow" >:: test_complete_workflow;
         "test_all_users_at_end" >:: test_all_users_at_end;
         "test_all_users_at_start" >:: test_all_users_at_start;
         "test_alternating_ops" >:: test_alternating_ops;
         "test_leapfrogging_users" >:: test_leapfrogging_users;
         "test_long_document" >:: test_long_document;
       ]

let () = run_test_tt_main suite
