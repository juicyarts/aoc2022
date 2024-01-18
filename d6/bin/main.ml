open List
open Str

let filename = Sys.argv.(1)

let count_substring substring str =
  let regex = regexp_string substring in
  let replaced_str = global_replace regex "" str in
  (String.length str - String.length replaced_str) / String.length substring

let p1 file seq_len =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  split (regexp "") (String.trim contents)
  |> fold_left
       (fun (i, j) (_ : string) ->
         match i with
         | _ when j > 0 -> (i + 1, j)
         | _ when i >= seq_len ->
             if
               for_all
                 (fun c ->
                   count_substring c (String.sub contents (i - seq_len) seq_len)
                   <= 1)
                 (split (regexp "") (String.sub contents (i - seq_len) seq_len))
             then (i + 1, i)
             else (i + 1, j)
         | _ -> (i + 1, j))
       (0, 0)

let () =
  let a, b = p1 filename 4 in
  print_endline (string_of_int b);
  print_endline (string_of_int a);

  let a, b = p1 filename 14 in
  print_endline (string_of_int b);
  print_endline (string_of_int a)
