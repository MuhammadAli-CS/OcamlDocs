(* bin/main.ml *)

let () =
  print_endline "Starting GUI client...";

  (* Start the Bogue GUI, connecting to localhost by default. *)
  Frontend.run_gui_client "127.0.0.1" 9000
