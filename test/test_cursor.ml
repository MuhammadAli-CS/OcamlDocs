open OUnit2
open Backend.Document
open Shared.Events

(* Helper Functions *)

let make_cursor id pos = { id; pos }
let make_cursors pairs = List.map (fun (id, pos) -> make_cursor id pos) pairs

(* Cursor Creation and Manipulation Tests *)

let test_cursor_creation _ =
  let cursor = make_cursor 42 10 in
  assert_equal 42 cursor.id ~printer:string_of_int;
  assert_equal 10 cursor.pos ~printer:string_of_int

let test_cursor_mutation _ =
  let cursor = make_cursor 1 5 in
  cursor.pos <- 10;
  assert_equal 10 cursor.pos ~printer:string_of_int;
  cursor.pos <- 0;
  assert_equal 0 cursor.pos ~printer:string_of_int

let test_multiple_cursors _ =
  let cursors = make_cursors [ (1, 0); (2, 5); (3, 10) ] in
  assert_equal 3 (List.length cursors) ~printer:string_of_int;
  assert_equal 0 (List.nth cursors 0).pos ~printer:string_of_int;
  assert_equal 5 (List.nth cursors 1).pos ~printer:string_of_int;
  assert_equal 10 (List.nth cursors 2).pos ~printer:string_of_int

(* ===== Single User Cursor Movement Tests ===== *)

let test_cursor_forward_on_insert _ =
  let doc = "test" in
  let cursor = make_cursor 1 2 in
  let _, _ = apply_local_change doc (KeyPressed 'X') 1 [ cursor ] in
  assert_equal 3 cursor.pos ~msg:"Cursor should advance after insert"
    ~printer:string_of_int

let test_cursor_backward_on_delete _ =
  let doc = "test" in
  let cursor = make_cursor 1 2 in
  let _, _ = apply_local_change doc Backspace 1 [ cursor ] in
  assert_equal 1 cursor.pos ~msg:"Cursor should move back after delete"
    ~printer:string_of_int

let test_cursor_no_move_on_failed_delete _ =
  let doc = "test" in
  let cursor = make_cursor 1 0 in
  let _, _ = apply_local_change doc Backspace 1 [ cursor ] in
  assert_equal 0 cursor.pos ~msg:"Cursor should stay at 0"
    ~printer:string_of_int

let test_cursor_set_explicit _ =
  let doc = "hello world" in
  let cursor = make_cursor 1 0 in
  let _, _ = apply_local_change doc (CursorSet 5) 1 [ cursor ] in
  assert_equal 5 cursor.pos ~printer:string_of_int;
  let _, _ = apply_local_change doc (CursorSet 11) 1 [ cursor ] in
  assert_equal 11 cursor.pos ~printer:string_of_int

let test_cursor_incremental_movement _ =
  let doc = "" in
  let cursor = make_cursor 1 0 in

  (* Insert characters and track cursor *)
  for i = 1 to 10 do
    let _, _ = apply_local_change doc (KeyPressed 'x') 1 [ cursor ] in
    assert_equal i cursor.pos
      ~msg:(Printf.sprintf "After insert %d" i)
      ~printer:string_of_int
  done

(* ===== Multi-User Cursor Shift Tests ===== *)

let test_two_cursors_insert_before _ =
  let doc = "test" in
  let c1 = make_cursor 1 0 in
  let c2 = make_cursor 2 4 in

  (* User 1 inserts at start *)
  let _, _ = apply_local_change doc (KeyPressed 'X') 1 [ c1; c2 ] in

  assert_equal 1 c1.pos ~msg:"Inserter moves forward";
  assert_equal 5 c2.pos ~msg:"User after insertion shifts right"

let test_two_cursors_insert_after _ =
  let doc = "test" in
  let c1 = make_cursor 1 0 in
  let c2 = make_cursor 2 4 in

  (* User 2 inserts at end *)
  let _, _ = apply_local_change doc (KeyPressed 'X') 2 [ c1; c2 ] in

  assert_equal 0 c1.pos ~msg:"User before insertion unchanged";
  assert_equal 5 c2.pos ~msg:"Inserter moves forward"

let test_two_cursors_delete_before _ =
  let doc = "test" in
  let c1 = make_cursor 1 1 in
  let c2 = make_cursor 2 4 in

  (* User 1 deletes at position 1 (deletes 't' at index 0) *)
  let _, _ = apply_local_change doc Backspace 1 [ c1; c2 ] in

  assert_equal 0 c1.pos ~msg:"Deleter moves back";
  assert_equal 3 c2.pos ~msg:"User after deletion shifts left"

let test_two_cursors_delete_after _ =
  let doc = "test" in
  let c1 = make_cursor 1 1 in
  let c2 = make_cursor 2 4 in

  (* User 2 deletes at position 4 (deletes 't' at index 3) *)
  let _, _ = apply_local_change doc Backspace 2 [ c1; c2 ] in

  assert_equal 1 c1.pos ~msg:"User before deletion unchanged";
  assert_equal 3 c2.pos ~msg:"Deleter moves back"

let test_three_cursors_middle_insert _ =
  let doc = "abcdef" in
  let cursors = make_cursors [ (1, 2); (2, 3); (3, 5) ] in

  (* User 2 inserts at position 3 *)
  let _, _ = apply_local_change doc (KeyPressed 'X') 2 cursors in

  assert_equal 2 (List.nth cursors 0).pos ~msg:"User before unchanged";
  assert_equal 4 (List.nth cursors 1).pos ~msg:"Inserter advances";
  assert_equal 6 (List.nth cursors 2).pos ~msg:"User after shifts"

let test_three_cursors_middle_delete _ =
  let doc = "abcdef" in
  let cursors = make_cursors [ (1, 2); (2, 4); (3, 6) ] in

  (* User 2 deletes at position 4 (deletes 'd' at index 3) *)
  let _, _ = apply_local_change doc Backspace 2 cursors in

  assert_equal 2 (List.nth cursors 0).pos ~msg:"User before unchanged";
  assert_equal 3 (List.nth cursors 1).pos ~msg:"Deleter moves back";
  assert_equal 5 (List.nth cursors 2).pos ~msg:"User after shifts left"

(* ===== Same Position Cursor Tests ===== *)

let test_cursors_at_same_position_insert _ =
  let doc = "test" in
  let c1 = make_cursor 1 2 in
  let c2 = make_cursor 2 2 in
  let c3 = make_cursor 3 2 in

  (* User 1 inserts *)
  let _, _ = apply_local_change doc (KeyPressed 'X') 1 [ c1; c2; c3 ] in

  (* All should be at position 3 *)
  assert_equal 3 c1.pos ~msg:"User 1";
  assert_equal 3 c2.pos ~msg:"User 2";
  assert_equal 3 c3.pos ~msg:"User 3"

let test_cursors_at_same_position_delete _ =
  let doc = "test" in
  let c1 = make_cursor 1 2 in
  let c2 = make_cursor 2 2 in

  (* User 1 deletes *)
  let _, _ = apply_local_change doc Backspace 1 [ c1; c2 ] in

  (* c1 deletes char at index 1, both end up at position 1 *)
  assert_equal 1 c1.pos ~msg:"User 1 deletes and moves back";
  assert_equal 1 c2.pos ~msg:"User 2 also shifts left (pos > deleted_idx)"

let test_adjacent_cursors_insert _ =
  let doc = "test" in
  let c1 = make_cursor 1 2 in
  let c2 = make_cursor 2 3 in

  (* User 1 inserts at 2 *)
  let _, _ = apply_local_change doc (KeyPressed 'X') 1 [ c1; c2 ] in

  assert_equal 3 c1.pos ~msg:"User 1 advances";
  assert_equal 4 c2.pos ~msg:"User 2 shifts right"

let test_adjacent_cursors_delete _ =
  let doc = "test" in
  let c1 = make_cursor 1 2 in
  let c2 = make_cursor 2 3 in

  (* User 2 deletes at 3 (deletes char at index 2) *)
  let _, _ = apply_local_change doc Backspace 2 [ c1; c2 ] in

  assert_equal 2 c1.pos ~msg:"User 1 unchanged (was at deletion point)";
  assert_equal 2 c2.pos ~msg:"User 2 moves back"

(* ===== Cursor Preservation Tests ===== *)

let test_cursor_preserved_across_operations _ =
  let doc = "test" in
  let cursor = make_cursor 1 2 in

  (* Perform multiple operations *)
  let doc, _ = apply_local_change doc (KeyPressed 'A') 1 [ cursor ] in
  assert_equal 3 cursor.pos;

  let doc, _ = apply_local_change doc (KeyPressed 'B') 1 [ cursor ] in
  assert_equal 4 cursor.pos;

  let doc, _ = apply_local_change doc Backspace 1 [ cursor ] in
  assert_equal 3 cursor.pos;

  assert_equal "teAst" doc ~printer:(fun s -> s)

let test_cursor_ids_preserved _ =
  let cursors = make_cursors [ (1, 0); (2, 5); (3, 10) ] in
  let doc = "hello world" in

  (* Perform an operation *)
  let _, _ = apply_local_change doc (KeyPressed 'X') 2 cursors in

  (* Check IDs are unchanged *)
  assert_equal 1 (List.nth cursors 0).id;
  assert_equal 2 (List.nth cursors 1).id;
  assert_equal 3 (List.nth cursors 2).id

(* Cursor Boundary Tests *)

let test_cursor_at_document_start _ =
  let doc = "test" in
  let cursor = make_cursor 1 0 in

  (* Insert at start *)
  let doc, _ = apply_local_change doc (KeyPressed 'X') 1 [ cursor ] in
  assert_equal "Xtest" doc ~printer:(fun s -> s);
  assert_equal 1 cursor.pos;

  (* Try to delete at start *)
  cursor.pos <- 0;
  let doc, _ = apply_local_change doc Backspace 1 [ cursor ] in
  assert_equal "Xtest" doc ~printer:(fun s -> s);
  assert_equal 0 cursor.pos

let test_cursor_at_document_end _ =
  let doc = "test" in
  let cursor = make_cursor 1 4 in

  (* Insert at end *)
  let doc, _ = apply_local_change doc (KeyPressed 'X') 1 [ cursor ] in
  assert_equal "testX" doc ~printer:(fun s -> s);
  assert_equal 5 cursor.pos;

  (* Delete from end *)
  let doc, _ = apply_local_change doc Backspace 1 [ cursor ] in
  assert_equal "test" doc ~printer:(fun s -> s);
  assert_equal 4 cursor.pos

let test_cursor_beyond_document _ =
  let doc = "test" in
  let cursor = make_cursor 1 100 in

  (* CursorSet should clamp *)
  let _, _ = apply_local_change doc (CursorSet 100) 1 [ cursor ] in
  assert_equal 4 cursor.pos ~msg:"Should clamp to doc length"

(* Complex Multi-User Cursor Scenarios *)

let test_cascading_cursor_shifts _ =
  let doc = "abcdefghij" in
  let cursors = make_cursors [ (1, 2); (2, 4); (3, 6); (4, 8) ] in

  (* User at position 2 inserts *)
  let _, _ = apply_local_change doc (KeyPressed 'X') 1 cursors in

  (* All users after should shift *)
  assert_equal 3 (List.nth cursors 0).pos;
  assert_equal 5 (List.nth cursors 1).pos;
  assert_equal 7 (List.nth cursors 2).pos;
  assert_equal 9 (List.nth cursors 3).pos

let test_reverse_cascading_shifts _ =
  let doc = "abcdefghij" in
  let cursors = make_cursors [ (1, 2); (2, 4); (3, 6); (4, 8) ] in

  (* User at position 4 deletes *)
  let _, _ = apply_local_change doc Backspace 2 cursors in

  (* Users before unchanged, users after shift left *)
  assert_equal 2 (List.nth cursors 0).pos;
  assert_equal 3 (List.nth cursors 1).pos ~msg:"Deleter moves back";
  assert_equal 5 (List.nth cursors 2).pos ~msg:"After shifts left";
  assert_equal 7 (List.nth cursors 3).pos ~msg:"After shifts left"

let test_interleaved_cursor_positions _ =
  let doc = "0123456789" in
  let cursors = make_cursors [ (1, 0); (2, 2); (3, 4); (4, 6); (5, 8) ] in

  (* User 3 (at pos 4) inserts *)
  let _, _ = apply_local_change doc (KeyPressed 'X') 3 cursors in

  assert_equal 0 (List.nth cursors 0).pos ~msg:"User 1 before";
  assert_equal 2 (List.nth cursors 1).pos ~msg:"User 2 before";
  assert_equal 5 (List.nth cursors 2).pos ~msg:"User 3 advances";
  assert_equal 7 (List.nth cursors 3).pos ~msg:"User 4 after";
  assert_equal 9 (List.nth cursors 4).pos ~msg:"User 5 after"

let test_all_cursors_move_together _ =
  let doc = "test" in
  let cursors = List.init 10 (fun i -> make_cursor (i + 1) 2) in

  (* User 5 inserts *)
  let _, _ = apply_local_change doc (KeyPressed 'X') 5 cursors in

  (* All cursors should be at position 3 *)
  List.iter
    (fun c ->
      assert_equal 3 c.pos
        ~msg:(Printf.sprintf "User %d" c.id)
        ~printer:string_of_int)
    cursors

(* Cursor Tracking Through Complex Edits *)

let test_cursor_through_word_insertion _ =
  let doc = "" in
  let cursor = make_cursor 1 0 in

  (* Type "hello" *)
  let chars = [ 'h'; 'e'; 'l'; 'l'; 'o' ] in
  let _ =
    List.fold_left
      (fun (d, pos) c ->
        let new_doc, _ = apply_local_change d (KeyPressed c) 1 [ cursor ] in
        assert_equal (pos + 1) cursor.pos
          ~msg:(Printf.sprintf "After typing '%c'" c);
        (new_doc, pos + 1))
      (doc, 0) chars
  in

  assert_equal 5 cursor.pos

let test_cursor_through_word_deletion _ =
  let doc = "hello" in
  let cursor = make_cursor 1 5 in

  (* Delete all *)
  for i = 5 downto 1 do
    let _, _ = apply_local_change doc Backspace 1 [ cursor ] in
    assert_equal (i - 1) cursor.pos
      ~msg:(Printf.sprintf "After deletion %d" (6 - i))
  done

let test_multiple_cursors_independent_movements _ =
  let doc = "test" in
  let c1 = make_cursor 1 0 in
  let c2 = make_cursor 2 2 in
  let c3 = make_cursor 3 4 in

  (* Move each cursor independently *)
  let _, _ = apply_local_change doc (CursorSet 1) 1 [ c1; c2; c3 ] in
  assert_equal 1 c1.pos;
  assert_equal 2 c2.pos ~msg:"Unchanged";
  assert_equal 4 c3.pos ~msg:"Unchanged";

  let _, _ = apply_local_change doc (CursorSet 3) 2 [ c1; c2; c3 ] in
  assert_equal 1 c1.pos ~msg:"Unchanged";
  assert_equal 3 c2.pos;
  assert_equal 4 c3.pos ~msg:"Unchanged"

(* Zero-Width Cursor Tests *)

let test_cursors_in_empty_doc _ =
  let doc = "" in
  let cursors = make_cursors [ (1, 0); (2, 0); (3, 0) ] in

  (* All at position 0 *)
  List.iter (fun c -> assert_equal 0 c.pos ~printer:string_of_int) cursors;

  (* User 1 types *)
  let _, _ = apply_local_change doc (KeyPressed 'X') 1 cursors in

  (* All should move to position 1 *)
  List.iter
    (fun c ->
      assert_equal 1 c.pos
        ~msg:(Printf.sprintf "User %d" c.id)
        ~printer:string_of_int)
    cursors

(* Test Suite *)

let suite =
  "Cursor Behavior Tests"
  >::: [
         (* Cursor creation *)
         "test_cursor_creation" >:: test_cursor_creation;
         "test_cursor_mutation" >:: test_cursor_mutation;
         "test_multiple_cursors" >:: test_multiple_cursors;
         (* Single user cursor movement *)
         "test_cursor_forward_on_insert" >:: test_cursor_forward_on_insert;
         "test_cursor_backward_on_delete" >:: test_cursor_backward_on_delete;
         "test_cursor_no_move_on_failed_delete"
         >:: test_cursor_no_move_on_failed_delete;
         "test_cursor_set_explicit" >:: test_cursor_set_explicit;
         "test_cursor_incremental_movement" >:: test_cursor_incremental_movement;
         (* Multi-user cursor shifts *)
         "test_two_cursors_insert_before" >:: test_two_cursors_insert_before;
         "test_two_cursors_insert_after" >:: test_two_cursors_insert_after;
         "test_two_cursors_delete_before" >:: test_two_cursors_delete_before;
         "test_two_cursors_delete_after" >:: test_two_cursors_delete_after;
         "test_three_cursors_middle_insert" >:: test_three_cursors_middle_insert;
         "test_three_cursors_middle_delete" >:: test_three_cursors_middle_delete;
         (* Same position cursors *)
         "test_cursors_at_same_position_insert"
         >:: test_cursors_at_same_position_insert;
         "test_cursors_at_same_position_delete"
         >:: test_cursors_at_same_position_delete;
         "test_adjacent_cursors_insert" >:: test_adjacent_cursors_insert;
         "test_adjacent_cursors_delete" >:: test_adjacent_cursors_delete;
         (* Cursor preservation *)
         "test_cursor_preserved_across_operations"
         >:: test_cursor_preserved_across_operations;
         "test_cursor_ids_preserved" >:: test_cursor_ids_preserved;
         (* Cursor boundaries *)
         "test_cursor_at_document_start" >:: test_cursor_at_document_start;
         "test_cursor_at_document_end" >:: test_cursor_at_document_end;
         "test_cursor_beyond_document" >:: test_cursor_beyond_document;
         (* Complex multi-user scenarios *)
         "test_cascading_cursor_shifts" >:: test_cascading_cursor_shifts;
         "test_reverse_cascading_shifts" >:: test_reverse_cascading_shifts;
         "test_interleaved_cursor_positions"
         >:: test_interleaved_cursor_positions;
         "test_all_cursors_move_together" >:: test_all_cursors_move_together;
         (* Cursor tracking *)
         "test_cursor_through_word_insertion"
         >:: test_cursor_through_word_insertion;
         "test_cursor_through_word_deletion"
         >:: test_cursor_through_word_deletion;
         "test_multiple_cursors_independent_movements"
         >:: test_multiple_cursors_independent_movements;
         (* Zero-width cursor *)
         "test_cursors_in_empty_doc" >:: test_cursors_in_empty_doc;
       ]

let () = run_test_tt_main suite
