open OUnit2
open Backend.Document
open Shared.Events

(* Helper Functions *)

let make_cursor id pos = { id; pos }

(* Stress Tests *)

let test_stress_1000_insertions _ =
  let doc = "" in
  let cursor = make_cursor 1 0 in
  let rec insert_many n doc cursor =
    if n = 0 then doc
    else
      let char = Char.chr (97 + (n mod 26)) in
      let new_doc, _ = apply_local_change doc (KeyPressed char) 1 [ cursor ] in
      insert_many (n - 1) new_doc cursor
  in
  let result = insert_many 1000 doc cursor in
  assert_equal 1000 (String.length result) ~printer:string_of_int;
  assert_equal 1000 cursor.pos ~printer:string_of_int

let test_stress_1000_deletions _ =
  let doc = String.make 1000 'x' in
  let cursor = make_cursor 1 1000 in
  let rec delete_many n doc cursor =
    if n = 0 then doc
    else
      let new_doc, update = apply_local_change doc Backspace 1 [ cursor ] in
      match update with
      | None -> doc
      | Some _ -> delete_many (n - 1) new_doc cursor
  in
  let result = delete_many 1000 doc cursor in
  assert_equal "" result ~printer:(fun s -> s);
  assert_equal 0 cursor.pos ~printer:string_of_int

let test_stress_alternating_ops_1000 _ =
  let doc = "" in
  let cursor = make_cursor 1 0 in
  let rec alternate n doc cursor =
    if n = 0 then doc
    else
      let doc, _ = apply_local_change doc (KeyPressed 'x') 1 [ cursor ] in
      let doc, _ = apply_local_change doc (KeyPressed 'y') 1 [ cursor ] in
      let doc, _ = apply_local_change doc Backspace 1 [ cursor ] in
      alternate (n - 1) doc cursor
  in
  let result = alternate 500 doc cursor in
  assert_equal 500 (String.length result) ~printer:string_of_int

let test_stress_10_users_100_ops_each _ =
  let doc = "" in
  let num_users = 10 in
  let cursors = List.init num_users (fun i -> make_cursor (i + 1) 0) in

  let rec all_users_type n doc =
    if n = 0 then doc
    else
      let doc =
        List.fold_left
          (fun d cursor ->
            let char = Char.chr (97 + (cursor.id mod 26)) in
            let new_doc, _ =
              apply_local_change d (KeyPressed char) cursor.id cursors
            in
            new_doc)
          doc cursors
      in
      all_users_type (n - 1) doc
  in

  let result = all_users_type 10 doc in
  assert_equal 100 (String.length result) ~msg:"10 users * 10 ops"
    ~printer:string_of_int

let test_stress_very_long_document _ =
  let doc = String.make 10000 'a' in
  let cursor = make_cursor 1 5000 in

  (* Insert in middle of very long doc *)
  let doc, _ = apply_local_change doc (KeyPressed 'X') 1 [ cursor ] in
  assert_equal 10001 (String.length doc) ~printer:string_of_int;
  assert_equal 5001 cursor.pos ~printer:string_of_int;

  (* Delete from middle *)
  let doc, _ = apply_local_change doc Backspace 1 [ cursor ] in
  assert_equal 10000 (String.length doc) ~printer:string_of_int;
  assert_equal 5000 cursor.pos ~printer:string_of_int

(* ===== Edge Case Tests ===== *)

let test_edge_empty_document_operations _ =
  let doc = "" in
  let cursor = make_cursor 1 0 in

  (* Try to delete from empty doc *)
  let doc, update = apply_local_change doc Backspace 1 [ cursor ] in
  assert_equal "" doc ~printer:(fun s -> s);
  assert_equal None update;
  assert_equal 0 cursor.pos;

  (* Set cursor in empty doc *)
  let doc, _ = apply_local_change doc (CursorSet 100) 1 [ cursor ] in
  assert_equal "" doc ~printer:(fun s -> s);
  assert_equal 0 cursor.pos ~msg:"Should clamp to 0"

let test_edge_single_char_operations _ =
  let doc = "x" in
  let cursor = make_cursor 1 1 in

  (* Delete the only char *)
  let doc, _ = apply_local_change doc Backspace 1 [ cursor ] in
  assert_equal "" doc ~printer:(fun s -> s);
  assert_equal 0 cursor.pos;

  (* Add it back *)
  let doc, _ = apply_local_change doc (KeyPressed 'x') 1 [ cursor ] in
  assert_equal "x" doc ~printer:(fun s -> s);
  assert_equal 1 cursor.pos

let test_edge_cursor_at_limits _ =
  let doc = "hello" in
  let cursor = make_cursor 1 0 in

  (* Cursor at 0, try to delete *)
  let doc, update = apply_local_change doc Backspace 1 [ cursor ] in
  assert_equal "hello" doc ~printer:(fun s -> s);
  assert_equal None update;

  (* Move to end *)
  cursor.pos <- 5;
  let doc, _ = apply_local_change doc (KeyPressed '!') 1 [ cursor ] in
  assert_equal "hello!" doc ~printer:(fun s -> s);
  assert_equal 6 cursor.pos

let test_edge_negative_cursor_position _ =
  let doc = "test" in
  let cursor = make_cursor 1 2 in

  (* Try to set cursor to negative position *)
  let doc, _ = apply_local_change doc (CursorSet (-10)) 1 [ cursor ] in
  assert_equal "test" doc ~printer:(fun s -> s);
  assert_equal 0 cursor.pos ~msg:"Should clamp to 0"

let test_edge_cursor_beyond_document _ =
  let doc = "test" in
  let cursor = make_cursor 1 2 in

  (* Try to set cursor beyond document *)
  let doc, _ = apply_local_change doc (CursorSet 1000) 1 [ cursor ] in
  assert_equal "test" doc ~printer:(fun s -> s);
  assert_equal 4 cursor.pos ~msg:"Should clamp to doc length"

let test_edge_all_users_at_same_pos _ =
  let doc = "abcd" in
  let cursors = List.init 10 (fun i -> make_cursor (i + 1) 2) in

  (* User 5 types *)
  let doc, _ = apply_local_change doc (KeyPressed 'X') 5 cursors in

  assert_equal "abXcd" doc ~printer:(fun s -> s);

  (* All cursors should be at position 3 *)
  List.iter
    (fun c ->
      assert_equal 3 c.pos
        ~msg:(Printf.sprintf "User %d" c.id)
        ~printer:string_of_int)
    cursors

let test_edge_interleaving_same_position _ =
  let doc = "test" in
  let c1 = make_cursor 1 2 in
  let c2 = make_cursor 2 2 in

  (* User 1 types *)
  let doc, _ = apply_local_change doc (KeyPressed 'A') 1 [ c1; c2 ] in
  assert_equal "teAst" doc ~printer:(fun s -> s);
  assert_equal 3 c1.pos;
  assert_equal 3 c2.pos;

  (* User 2 types immediately *)
  let doc, _ = apply_local_change doc (KeyPressed 'B') 2 [ c1; c2 ] in
  assert_equal "teABst" doc ~printer:(fun s -> s);
  assert_equal 4 c1.pos;
  assert_equal 4 c2.pos

let test_edge_rapid_cursor_movements _ =
  let doc = "0123456789" in
  let cursor = make_cursor 1 0 in

  (* Jump around rapidly *)
  let _, _ = apply_local_change doc (CursorSet 5) 1 [ cursor ] in
  assert_equal 5 cursor.pos;

  let _, _ = apply_local_change doc (CursorSet 0) 1 [ cursor ] in
  assert_equal 0 cursor.pos;

  let _, _ = apply_local_change doc (CursorSet 10) 1 [ cursor ] in
  assert_equal 10 cursor.pos;

  let _, _ = apply_local_change doc (CursorSet 3) 1 [ cursor ] in
  assert_equal 3 cursor.pos

let test_edge_delete_at_position_0 _ =
  let doc = "hello" in
  let cursor = make_cursor 1 0 in

  let doc, update = apply_local_change doc Backspace 1 [ cursor ] in
  assert_equal "hello" doc ~msg:"No change" ~printer:(fun s -> s);
  assert_equal None update;
  assert_equal 0 cursor.pos

let test_edge_insert_at_every_position _ =
  let doc = "ace" in
  let cursor = make_cursor 1 0 in

  (* Insert at position 0 *)
  let doc, _ = apply_local_change doc (KeyPressed 'X') 1 [ cursor ] in
  assert_equal "Xace" doc ~printer:(fun s -> s);

  (* Move and insert at position 2 *)
  let _, _ = apply_local_change doc (CursorSet 2) 1 [ cursor ] in
  let doc, _ = apply_local_change doc (KeyPressed 'Y') 1 [ cursor ] in
  assert_equal "XaYce" doc ~printer:(fun s -> s);

  (* Move and insert at end *)
  let _, _ = apply_local_change doc (CursorSet 5) 1 [ cursor ] in
  let doc, _ = apply_local_change doc (KeyPressed 'Z') 1 [ cursor ] in
  assert_equal "XaYceZ" doc ~printer:(fun s -> s)

let test_edge_delete_from_every_position _ =
  let doc = "abcde" in
  let cursor = make_cursor 1 5 in

  (* Delete from end *)
  let doc, _ = apply_local_change doc Backspace 1 [ cursor ] in
  assert_equal "abcd" doc ~printer:(fun s -> s);

  (* Delete from position 2 *)
  let _, _ = apply_local_change doc (CursorSet 2) 1 [ cursor ] in
  let doc, _ = apply_local_change doc Backspace 1 [ cursor ] in
  assert_equal "acd" doc ~printer:(fun s -> s);

  (* Delete from position 1 *)
  let _, _ = apply_local_change doc (CursorSet 1) 1 [ cursor ] in
  let doc, _ = apply_local_change doc Backspace 1 [ cursor ] in
  assert_equal "cd" doc ~printer:(fun s -> s)

(* ===== Special Character Tests ===== *)

let test_special_newline _ =
  let doc = "" in
  let cursor = make_cursor 1 0 in
  let doc, _ = apply_local_change doc (KeyPressed '\n') 1 [ cursor ] in
  assert_equal "\n" doc ~printer:(fun s -> Printf.sprintf "<%s>" s);
  assert_equal 1 (String.length doc) ~printer:string_of_int

let test_special_tab _ =
  let doc = "" in
  let cursor = make_cursor 1 0 in
  let doc, _ = apply_local_change doc (KeyPressed '\t') 1 [ cursor ] in
  assert_equal "\t" doc ~printer:(fun s -> Printf.sprintf "<%s>" s);
  assert_equal 1 (String.length doc) ~printer:string_of_int

let test_special_null_char _ =
  let doc = "" in
  let cursor = make_cursor 1 0 in
  let doc, _ = apply_local_change doc (KeyPressed '\000') 1 [ cursor ] in
  assert_equal 1 (String.length doc) ~printer:string_of_int

let test_special_all_whitespace _ =
  let doc = "" in
  let cursor = make_cursor 1 0 in
  let doc, _ = apply_local_change doc (KeyPressed ' ') 1 [ cursor ] in
  let doc, _ = apply_local_change doc (KeyPressed '\t') 1 [ cursor ] in
  let doc, _ = apply_local_change doc (KeyPressed '\n') 1 [ cursor ] in
  let doc, _ = apply_local_change doc (KeyPressed ' ') 1 [ cursor ] in
  assert_equal 4 (String.length doc) ~printer:string_of_int

let test_special_repeated_chars _ =
  let doc = "" in
  let cursor = make_cursor 1 0 in
  let rec insert_same n doc cursor =
    if n = 0 then doc
    else
      let new_doc, _ = apply_local_change doc (KeyPressed 'z') 1 [ cursor ] in
      insert_same (n - 1) new_doc cursor
  in
  let result = insert_same 100 doc cursor in
  assert_equal (String.make 100 'z') result ~printer:(fun s -> s)

(* ===== Boundary Tests ===== *)

let test_boundary_max_int_cursor _ =
  let doc = "test" in
  let cursor = make_cursor 1 0 in
  let _, _ = apply_local_change doc (CursorSet max_int) 1 [ cursor ] in
  assert_equal 4 cursor.pos ~msg:"Should clamp to doc length"
    ~printer:string_of_int

let test_boundary_min_int_cursor _ =
  let doc = "test" in
  let cursor = make_cursor 1 4 in
  let _, _ = apply_local_change doc (CursorSet min_int) 1 [ cursor ] in
  assert_equal 0 cursor.pos ~msg:"Should clamp to 0" ~printer:string_of_int

let test_boundary_many_users_one_position _ =
  let doc = "test" in
  let num_users = 100 in
  let cursors = List.init num_users (fun i -> make_cursor (i + 1) 2) in

  (* User 50 types *)
  let doc, _ = apply_local_change doc (KeyPressed 'X') 50 cursors in

  assert_equal "teXst" doc ~printer:(fun s -> s);

  (* All cursors at position 3 *)
  List.iter (fun c -> assert_equal 3 c.pos ~printer:string_of_int) cursors

let test_boundary_zero_length_after_operations _ =
  let doc = "x" in
  let cursor = make_cursor 1 1 in
  let doc, _ = apply_local_change doc Backspace 1 [ cursor ] in

  assert_equal 0 (length doc) ~printer:string_of_int;
  assert_equal "" doc ~printer:(fun s -> s);

  (* Try operations on zero-length doc *)
  let doc, update = apply_local_change doc Backspace 1 [ cursor ] in
  assert_equal "" doc ~printer:(fun s -> s);
  assert_equal None update

(* ===== Error Handling Tests ===== *)

let test_error_nonexistent_user _ =
  let doc = "test" in
  let c1 = make_cursor 1 0 in

  (* Try to apply action for non-existent user *)
  let doc, update = apply_local_change doc (KeyPressed 'X') 999 [ c1 ] in

  assert_equal "test" doc ~msg:"Doc should not change" ~printer:(fun s -> s);
  assert_equal None update;
  assert_equal 0 c1.pos ~msg:"Other cursors unchanged"

let test_error_empty_cursor_list _ =
  let doc = "test" in

  (* Apply action with empty cursor list *)
  let doc, update = apply_local_change doc (KeyPressed 'X') 1 [] in

  assert_equal "test" doc ~msg:"Doc should not change" ~printer:(fun s -> s);
  assert_equal None update

(* ===== Consistency Tests ===== *)

let test_consistency_insert_delete_insert _ =
  let doc = "test" in
  let cursor = make_cursor 1 4 in

  let doc, _ = apply_local_change doc (KeyPressed 'A') 1 [ cursor ] in
  let doc, _ = apply_local_change doc Backspace 1 [ cursor ] in
  let doc, _ = apply_local_change doc (KeyPressed 'B') 1 [ cursor ] in

  assert_equal "testB" doc ~printer:(fun s -> s);
  assert_equal 5 cursor.pos

let test_consistency_multiple_backspaces _ =
  let doc = "hello" in
  let cursor = make_cursor 1 5 in

  (* Delete all characters *)
  let doc, _ = apply_local_change doc Backspace 1 [ cursor ] in
  let doc, _ = apply_local_change doc Backspace 1 [ cursor ] in
  let doc, _ = apply_local_change doc Backspace 1 [ cursor ] in
  let doc, _ = apply_local_change doc Backspace 1 [ cursor ] in
  let doc, _ = apply_local_change doc Backspace 1 [ cursor ] in

  assert_equal "" doc ~printer:(fun s -> s);
  assert_equal 0 cursor.pos;

  (* Try to delete more *)
  let doc, update = apply_local_change doc Backspace 1 [ cursor ] in
  assert_equal "" doc ~printer:(fun s -> s);
  assert_equal None update

let test_consistency_cursor_tracking _ =
  let doc = "abc" in
  let c1 = make_cursor 1 1 in
  let c2 = make_cursor 2 2 in
  let c3 = make_cursor 3 3 in

  (* Track positions through multiple operations *)
  let doc, _ = apply_local_change doc (KeyPressed 'X') 1 [ c1; c2; c3 ] in
  assert_equal 2 c1.pos;
  assert_equal 3 c2.pos;
  assert_equal 4 c3.pos;

  let doc, _ = apply_local_change doc Backspace 2 [ c1; c2; c3 ] in
  assert_equal 2 c1.pos ~msg:"User 1 unchanged";
  assert_equal 2 c2.pos ~msg:"User 2 moved back";
  assert_equal 3 c3.pos ~msg:"User 3 shifted left";

  assert_equal "aXc" doc ~printer:(fun s -> s)

(* ===== Test Suite ===== *)

let suite =
  "Edge Cases and Stress Tests"
  >::: [
         (* Stress tests *)
         "test_stress_1000_insertions" >:: test_stress_1000_insertions;
         "test_stress_1000_deletions" >:: test_stress_1000_deletions;
         "test_stress_alternating_ops_1000" >:: test_stress_alternating_ops_1000;
         "test_stress_10_users_100_ops_each"
         >:: test_stress_10_users_100_ops_each;
         "test_stress_very_long_document" >:: test_stress_very_long_document;
         (* Edge cases *)
         "test_edge_empty_document_operations"
         >:: test_edge_empty_document_operations;
         "test_edge_single_char_operations" >:: test_edge_single_char_operations;
         "test_edge_cursor_at_limits" >:: test_edge_cursor_at_limits;
         "test_edge_negative_cursor_position"
         >:: test_edge_negative_cursor_position;
         "test_edge_cursor_beyond_document" >:: test_edge_cursor_beyond_document;
         "test_edge_all_users_at_same_pos" >:: test_edge_all_users_at_same_pos;
         "test_edge_interleaving_same_position"
         >:: test_edge_interleaving_same_position;
         "test_edge_rapid_cursor_movements" >:: test_edge_rapid_cursor_movements;
         "test_edge_delete_at_position_0" >:: test_edge_delete_at_position_0;
         "test_edge_insert_at_every_position"
         >:: test_edge_insert_at_every_position;
         "test_edge_delete_from_every_position"
         >:: test_edge_delete_from_every_position;
         (* Special characters *)
         "test_special_newline" >:: test_special_newline;
         "test_special_tab" >:: test_special_tab;
         "test_special_null_char" >:: test_special_null_char;
         "test_special_all_whitespace" >:: test_special_all_whitespace;
         "test_special_repeated_chars" >:: test_special_repeated_chars;
         (* Boundary tests *)
         "test_boundary_max_int_cursor" >:: test_boundary_max_int_cursor;
         "test_boundary_min_int_cursor" >:: test_boundary_min_int_cursor;
         "test_boundary_many_users_one_position"
         >:: test_boundary_many_users_one_position;
         "test_boundary_zero_length_after_operations"
         >:: test_boundary_zero_length_after_operations;
         (* Error handling *)
         "test_error_nonexistent_user" >:: test_error_nonexistent_user;
         "test_error_empty_cursor_list" >:: test_error_empty_cursor_list;
         (* Consistency tests *)
         "test_consistency_insert_delete_insert"
         >:: test_consistency_insert_delete_insert;
         "test_consistency_multiple_backspaces"
         >:: test_consistency_multiple_backspaces;
         "test_consistency_cursor_tracking" >:: test_consistency_cursor_tracking;
       ]

let () = run_test_tt_main suite
