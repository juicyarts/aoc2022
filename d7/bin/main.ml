open List
open Str

let filename = Sys.argv.(1)

let parse_command command repl =
  let result = global_replace (regexp_string repl) "" command in
  result

let parse_file command = split (regexp " ") command

let rec get_dir_size (dir : string) (init_size : int) dirs =
  Hashtbl.fold
    (fun d (parent, is_dir, size) i ->
      match i with
      | _ when d = dir -> i
      | _ when parent = dir ->
          if is_dir then get_dir_size d i dirs else i + size
      | _ -> i)
    dirs init_size

let index_tree (dirs, cd) command =
  match command with
  | _ when String.starts_with ~prefix:"$ cd " command ->
      if parse_command command "$ cd " = ".." then
        ( dirs,
          let x, _, _ = Hashtbl.find dirs cd in
          x )
      else
        let path = cd ^ parse_command command "$ cd " in
        Hashtbl.add dirs path (cd, true, 0);
        (dirs, path)
  | _ when String.starts_with ~prefix:"$ ls" command -> (dirs, cd)
  | _ when String.starts_with ~prefix:"dir " command ->
      let path = parse_command command "dir " in
      Hashtbl.add dirs path (cd, true, 0);
      (dirs, cd)
  | _ ->
      let path = cd ^ nth (parse_file command) 1 in
      Hashtbl.add dirs path
        (cd, false, int_of_string (nth (parse_file command) 0));
      (dirs, cd)

let p1 file =
  let content = In_channel.with_open_bin file In_channel.input_all in
  let tree, _ =
    split (regexp "\n") content |> fold_left index_tree (Hashtbl.create 100, "/")
  in
  Hashtbl.fold
    (fun k (_, is_dir, _) i ->
      let size = get_dir_size k 0 tree in
      if is_dir && size <= 100000 then i + size else i)
    tree 0

let p2 file =
  let content = In_channel.with_open_bin file In_channel.input_all in
  let tree, _ =
    split (regexp "\n") content |> fold_left index_tree (Hashtbl.create 100, "/")
  in
  let total = get_dir_size "/" 0 tree in
  let to_free = 30000000 - (70000000 - total) in
  Hashtbl.fold
    (fun k (_, is_dir, _) i ->
      let size = get_dir_size k 0 tree in
      if is_dir && size >= to_free && i > size then size else i)
    tree total

let () =
  let a = p1 filename in
  print_endline (string_of_int a);
  let b = p2 filename in
  print_endline (string_of_int b)
