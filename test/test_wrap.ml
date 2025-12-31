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

let print_wrapped s w =
  Printf.printf "Wrapping '%s' at %d:\n" s w;
  List.iter
    (fun l -> Printf.printf "  '%s' (len %d)\n" l (String.length l))
    (wrap_string s w);
  print_newline ()

let () =
  print_wrapped "" 10;
  print_wrapped "hello" 10;
  print_wrapped "hello world" 5;
  print_wrapped "helloworld" 5;
  print_wrapped "hello world" 11;
  print_wrapped "a b c d e f" 3;
  print_wrapped "   " 2;
  print_wrapped "longstringwithoutspaces" 5
