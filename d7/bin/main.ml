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
      else (
        if Hashtbl.mem dirs (parse_command command "$ cd ") = true then ()
        else Hashtbl.add dirs (parse_command command "$ cd ") (cd, true, 0);
        (dirs, parse_command command "$ cd "))
  | _ when String.starts_with ~prefix:"$ ls" command -> (dirs, cd)
  | _ when String.starts_with ~prefix:"dir " command ->
      if Hashtbl.mem dirs (parse_command command "dir ") = true then ()
      else Hashtbl.add dirs (parse_command command "dir ") (cd, true, 0);
      (dirs, cd)
  | _ ->
      Hashtbl.add dirs
        (nth (parse_file command) 1)
        (cd, false, int_of_string (nth (parse_file command) 0));
      (dirs, cd)

let rec render tree dir depth =
  let dirsize = get_dir_size dir 0 tree in

  if dirsize <= 100000 then
    print_endline
      (Printf.sprintf "%*s- %s (dir, size=%s)" depth "" dir
         (string_of_int dirsize))
  else ();
  Hashtbl.iter
    (fun k (parent, is_dir, _) ->
      match k with
      | _ when k = dir -> ()
      | _ when parent = dir ->
          if is_dir then render tree k (depth + 2)
          else ()
            (*print_endline
              (Printf.sprintf "%*s- %s" (depth + 2) "" k
              ^ " (file, size=" ^ string_of_int size ^ ")")*)
      | _ -> ())
    tree

let p1 file =
  let content = In_channel.with_open_bin file In_channel.input_all in
  let tree, _ =
    split (regexp "\n") content |> fold_left index_tree (Hashtbl.create 100, "/")
  in
  render tree "/" 0;
  Hashtbl.fold
    (fun k (_, is_dir, _) i ->
      let size = get_dir_size k 0 tree in
      if is_dir && size <= 100000 then i + size else i)
    tree 0

let () =
  let a = p1 filename in
  print_endline (string_of_int a)
