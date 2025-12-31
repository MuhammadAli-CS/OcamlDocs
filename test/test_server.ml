open OUnit2
open Shared.Events
module Server = Backend.Server
module Document = Backend.Document

(* We create a fake client that sends output to nowhere (Lwt_io.null) so we can
   test logic without real networking. *)
let make_dummy_client id =
  {
    Server.id;
    out_chan = Lwt_io.null;
    cursor_obj = { Document.id; pos = 0 };
    mutex = Lwt_mutex.create ();
  }

let test_protocol_parsing _ =
  (* 1. Test String Generation (Server -> Client) *)
  assert_equal "KEY 65" (Server.string_of_user_action (KeyPressed 'A'));
  assert_equal "BACKSPACE" (Server.string_of_user_action Backspace);
  assert_equal "SAVE" (Server.string_of_user_action SaveRequest);
  assert_equal "SET 10" (Server.string_of_user_action (CursorSet 10));

  (* 2. Test String Parsing (Client -> Server) *)
  assert_equal (Some Backspace) (Server.user_action_of_string "BACKSPACE");
  assert_equal (Some SaveRequest) (Server.user_action_of_string "SAVE");
  assert_equal (Some (KeyPressed 'B')) (Server.user_action_of_string "KEY 66");
  assert_equal (Some (CursorSet 5)) (Server.user_action_of_string "SET 5");

  (* Test Edge Cases *)
  assert_equal None (Server.user_action_of_string "KEY 999");
  (* Invalid Char *)
  assert_equal None (Server.user_action_of_string "GARBAGE");

  (* Bad Protocol *)

  (* 3. Test Backend Update Formatting *)
  assert_equal "FULL Hi" (Server.string_of_backend_update (FullDocument "Hi"));
  assert_equal "DOC Hi" (Server.string_of_backend_update (DocumentChanged "Hi"))

let test_apply_action_logic _ =
  (* Create a dummy client *)
  let cl = make_dummy_client 99 in

  (* 2. Test Save (Should return None and write file) *)
  let res_save = Server.apply_action cl SaveRequest in
  assert_equal None res_save

let suite =
  "Server Logic Tests"
  >::: [
         "protocol_parsing" >:: test_protocol_parsing;
         "apply_action_logic" >:: test_apply_action_logic;
       ]

let () = run_test_tt_main suite
