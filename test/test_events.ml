open OUnit2
open Shared.Events

(* User Action Type Tests *)

let test_keypressed_creation _ =
  let action = KeyPressed 'a' in
  match action with
  | KeyPressed c -> assert_equal 'a' c ~printer:(String.make 1)
  | _ -> assert_failure "Expected KeyPressed"

let test_backspace_creation _ =
  let action = Backspace in
  match action with
  | Backspace -> assert_bool "Success" true
  | _ -> assert_failure "Expected Backspace"

let test_cursorset_creation _ =
  let action = CursorSet 42 in
  match action with
  | CursorSet pos -> assert_equal 42 pos ~printer:string_of_int
  | _ -> assert_failure "Expected CursorSet"

let test_saverequest_creation _ =
  let action = SaveRequest in
  match action with
  | SaveRequest -> assert_bool "Success" true
  | _ -> assert_failure "Expected SaveRequest"

let test_alignment_creation _ =
  let action = AlignmentChanged "center" in
  match action with
  | AlignmentChanged alignment ->
      assert_equal "center" alignment ~printer:(fun s -> s)
  | _ -> assert_failure "Expected AlignmentChanged"

(* ===== Backend Update Type Tests ===== *)

let test_full_document_creation _ =
  let update = FullDocument "hello world" in
  match update with
  | FullDocument doc -> assert_equal "hello world" doc ~printer:(fun s -> s)
  | _ -> assert_failure "Expected FullDocument"

let test_document_changed_creation _ =
  let update = DocumentChanged "new text" in
  match update with
  | DocumentChanged doc -> assert_equal "new text" doc ~printer:(fun s -> s)
  | _ -> assert_failure "Expected DocumentChanged"

let test_cursor_moved_creation _ =
  let update = CursorMoved 15 in
  match update with
  | CursorMoved pos -> assert_equal 15 pos ~printer:string_of_int
  | _ -> assert_failure "Expected CursorMoved"

let test_alignment_update_creation _ =
  let update = AlignmentUpdate "right" in
  match update with
  | AlignmentUpdate alignment ->
      assert_equal "right" alignment ~printer:(fun s -> s)
  | _ -> assert_failure "Expected AlignmentUpdate"

(* ===== Pattern Matching Tests ===== *)

let test_match_all_user_actions _ =
  let actions =
    [
      KeyPressed 'x';
      Backspace;
      CursorSet 10;
      SaveRequest;
      AlignmentChanged "left";
    ]
  in
  let results =
    List.map
      (fun action ->
        match action with
        | KeyPressed _ -> "key"
        | Backspace -> "backspace"
        | CursorSet _ -> "cursor"
        | SaveRequest -> "save"
        | AlignmentChanged _ -> "alignment")
      actions
  in
  assert_equal [ "key"; "backspace"; "cursor"; "save"; "alignment" ] results

let test_match_all_backend_updates _ =
  let updates =
    [
      FullDocument "doc";
      DocumentChanged "doc2";
      CursorMoved 5;
      AlignmentUpdate "center";
    ]
  in
  let results =
    List.map
      (fun update ->
        match update with
        | FullDocument _ -> "full"
        | DocumentChanged _ -> "changed"
        | CursorMoved _ -> "cursor"
        | AlignmentUpdate _ -> "alignment")
      updates
  in
  assert_equal [ "full"; "changed"; "cursor"; "alignment" ] results

(* ===== Edge Cases and Special Values ===== *)

let test_keypressed_special_chars _ =
  let actions =
    [ KeyPressed '\n'; KeyPressed '\t'; KeyPressed ' '; KeyPressed '\000' ]
  in
  List.iter
    (fun action ->
      match action with
      | KeyPressed _ -> ()
      | _ -> assert_failure "All should be KeyPressed")
    actions

let test_cursorset_boundary_values _ =
  let actions = [ CursorSet 0; CursorSet max_int; CursorSet (-1) ] in
  let positions =
    List.map
      (fun action ->
        match action with
        | CursorSet pos -> pos
        | _ -> assert_failure "Expected CursorSet")
      actions
  in
  assert_equal [ 0; max_int; -1 ] positions

let test_empty_document_updates _ =
  let updates = [ FullDocument ""; DocumentChanged "" ] in
  List.iter
    (fun update ->
      match update with
      | FullDocument doc | DocumentChanged doc ->
          assert_equal "" doc ~printer:(fun s -> s)
      | _ -> assert_failure "Expected document update")
    updates

let test_long_document_updates _ =
  let long_text = String.make 10000 'x' in
  let update = FullDocument long_text in
  match update with
  | FullDocument doc ->
      assert_equal 10000 (String.length doc) ~printer:string_of_int
  | _ -> assert_failure "Expected FullDocument"

let test_alignment_values _ =
  let alignments = [ "left"; "center"; "right"; "justify"; "custom" ] in
  List.iter
    (fun align ->
      let action = AlignmentChanged align in
      match action with
      | AlignmentChanged a -> assert_equal align a ~printer:(fun s -> s)
      | _ -> assert_failure "Expected AlignmentChanged")
    alignments

(* ===== Equality Tests ===== *)

let test_keypressed_equality _ =
  let action1 = KeyPressed 'a' in
  let action2 = KeyPressed 'a' in
  let action3 = KeyPressed 'b' in
  assert_equal action1 action2;
  assert_bool "Should not be equal" (action1 <> action3)

let test_backspace_equality _ =
  let action1 = Backspace in
  let action2 = Backspace in
  assert_equal action1 action2

let test_cursorset_equality _ =
  let action1 = CursorSet 42 in
  let action2 = CursorSet 42 in
  let action3 = CursorSet 43 in
  assert_equal action1 action2;
  assert_bool "Should not be equal" (action1 <> action3)

let test_saverequest_equality _ =
  let action1 = SaveRequest in
  let action2 = SaveRequest in
  assert_equal action1 action2

let test_alignment_equality _ =
  let action1 = AlignmentChanged "left" in
  let action2 = AlignmentChanged "left" in
  let action3 = AlignmentChanged "right" in
  assert_equal action1 action2;
  assert_bool "Should not be equal" (action1 <> action3)

let test_full_document_equality _ =
  let update1 = FullDocument "test" in
  let update2 = FullDocument "test" in
  let update3 = FullDocument "other" in
  assert_equal update1 update2;
  assert_bool "Should not be equal" (update1 <> update3)

let test_document_changed_equality _ =
  let update1 = DocumentChanged "test" in
  let update2 = DocumentChanged "test" in
  let update3 = DocumentChanged "other" in
  assert_equal update1 update2;
  assert_bool "Should not be equal" (update1 <> update3)

let test_cursor_moved_equality _ =
  let update1 = CursorMoved 10 in
  let update2 = CursorMoved 10 in
  let update3 = CursorMoved 20 in
  assert_equal update1 update2;
  assert_bool "Should not be equal" (update1 <> update3)

let test_alignment_update_equality _ =
  let update1 = AlignmentUpdate "center" in
  let update2 = AlignmentUpdate "center" in
  let update3 = AlignmentUpdate "left" in
  assert_equal update1 update2;
  assert_bool "Should not be equal" (update1 <> update3)

(* ===== Collection Tests ===== *)

let test_list_of_user_actions _ =
  let actions =
    [
      KeyPressed 'h';
      KeyPressed 'e';
      KeyPressed 'l';
      KeyPressed 'l';
      KeyPressed 'o';
    ]
  in
  assert_equal 5 (List.length actions) ~printer:string_of_int

let test_list_of_mixed_actions _ =
  let actions =
    [ KeyPressed 'a'; Backspace; KeyPressed 'b'; CursorSet 0; SaveRequest ]
  in
  assert_equal 5 (List.length actions) ~printer:string_of_int

let test_filter_keypressed _ =
  let actions =
    [ KeyPressed 'a'; Backspace; KeyPressed 'b'; CursorSet 0; KeyPressed 'c' ]
  in
  let key_actions =
    List.filter
      (fun action ->
        match action with
        | KeyPressed _ -> true
        | _ -> false)
      actions
  in
  assert_equal 3 (List.length key_actions) ~printer:string_of_int

let test_map_cursor_positions _ =
  let actions = [ CursorSet 0; CursorSet 5; CursorSet 10 ] in
  let positions =
    List.filter_map
      (fun action ->
        match action with
        | CursorSet pos -> Some pos
        | _ -> None)
      actions
  in
  assert_equal [ 0; 5; 10 ] positions

(* ===== Type Safety Tests ===== *)

let test_user_action_is_variant _ =
  let action : user_action = KeyPressed 'x' in
  match action with
  | KeyPressed _ | Backspace | CursorSet _ | SaveRequest | AlignmentChanged _ ->
      assert_bool "Is a valid variant" true

let test_backend_update_is_variant _ =
  let update : backend_update = FullDocument "test" in
  match update with
  | FullDocument _ | DocumentChanged _ | CursorMoved _ | AlignmentUpdate _ ->
      assert_bool "Is a valid variant" true

(* ===== Comprehensive Coverage Tests ===== *)

let test_all_printable_ascii_keypressed _ =
  for code = 32 to 126 do
    let c = Char.chr code in
    let action = KeyPressed c in
    match action with
    | KeyPressed actual -> assert_equal c actual ~printer:(String.make 1)
    | _ -> assert_failure (Printf.sprintf "Failed for char %c" c)
  done

let test_cursor_positions_range _ =
  for pos = 0 to 100 do
    let action = CursorSet pos in
    match action with
    | CursorSet actual -> assert_equal pos actual ~printer:string_of_int
    | _ -> assert_failure (Printf.sprintf "Failed for position %d" pos)
  done

let test_various_alignment_strings _ =
  let test_cases =
    [
      "left";
      "center";
      "right";
      "justify";
      "start";
      "end";
      "LEFT";
      "CENTER";
      "custom-align-123";
    ]
  in
  List.iter
    (fun align_str ->
      let action = AlignmentChanged align_str in
      match action with
      | AlignmentChanged actual ->
          assert_equal align_str actual ~printer:(fun s -> s)
      | _ ->
          assert_failure (Printf.sprintf "Failed for alignment '%s'" align_str))
    test_cases

(* ===== Test Suite ===== *)

let suite =
  "Events Module Tests"
  >::: [
         (* User action creation *)
         "test_keypressed_creation" >:: test_keypressed_creation;
         "test_backspace_creation" >:: test_backspace_creation;
         "test_cursorset_creation" >:: test_cursorset_creation;
         "test_saverequest_creation" >:: test_saverequest_creation;
         "test_alignment_creation" >:: test_alignment_creation;
         (* Backend update creation *)
         "test_full_document_creation" >:: test_full_document_creation;
         "test_document_changed_creation" >:: test_document_changed_creation;
         "test_cursor_moved_creation" >:: test_cursor_moved_creation;
         "test_alignment_update_creation" >:: test_alignment_update_creation;
         (* Pattern matching *)
         "test_match_all_user_actions" >:: test_match_all_user_actions;
         "test_match_all_backend_updates" >:: test_match_all_backend_updates;
         (* Edge cases *)
         "test_keypressed_special_chars" >:: test_keypressed_special_chars;
         "test_cursorset_boundary_values" >:: test_cursorset_boundary_values;
         "test_empty_document_updates" >:: test_empty_document_updates;
         "test_long_document_updates" >:: test_long_document_updates;
         "test_alignment_values" >:: test_alignment_values;
         (* Equality *)
         "test_keypressed_equality" >:: test_keypressed_equality;
         "test_backspace_equality" >:: test_backspace_equality;
         "test_cursorset_equality" >:: test_cursorset_equality;
         "test_saverequest_equality" >:: test_saverequest_equality;
         "test_alignment_equality" >:: test_alignment_equality;
         "test_full_document_equality" >:: test_full_document_equality;
         "test_document_changed_equality" >:: test_document_changed_equality;
         "test_cursor_moved_equality" >:: test_cursor_moved_equality;
         "test_alignment_update_equality" >:: test_alignment_update_equality;
         (* Collections *)
         "test_list_of_user_actions" >:: test_list_of_user_actions;
         "test_list_of_mixed_actions" >:: test_list_of_mixed_actions;
         "test_filter_keypressed" >:: test_filter_keypressed;
         "test_map_cursor_positions" >:: test_map_cursor_positions;
         (* Type safety *)
         "test_user_action_is_variant" >:: test_user_action_is_variant;
         "test_backend_update_is_variant" >:: test_backend_update_is_variant;
         (* Comprehensive coverage *)
         "test_all_printable_ascii_keypressed"
         >:: test_all_printable_ascii_keypressed;
         "test_cursor_positions_range" >:: test_cursor_positions_range;
         "test_various_alignment_strings" >:: test_various_alignment_strings;
       ]

let () = run_test_tt_main suite
