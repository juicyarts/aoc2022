open List
open Str

let filename = Sys.argv.(1)

let alphabet = List.init 52 (fun i ->
  if i < 26 then (i+1, Char.chr (i + Char.code 'a'))
  else (i+1, (Char.chr (i - 26 + Char.code 'A'))))

let split_sack a = [(string_before a (String.length(a) / 2)); (string_after a (String.length(a) / 2)); ""]
let compare_compartments (a: string list) =
  List.init (String.length (nth a 0))  (String.get (nth a 0))
  |> fold_left (fun x c -> if (String.contains (nth a 1) c) then ((List.find (fun (_,a) -> a = c ) alphabet)) else x ) (nth alphabet 0)

let p1 file =
    In_channel.with_open_bin file In_channel.input_all
    |> split (regexp "\n")
    |> map (fun line -> (split_sack line) |> compare_compartments)
    |> fold_left (fun a i -> a + (fst i)) 0


let split_g a =
  split (regexp "^(?:.*\n){3}") a

let process_g g =
  split (regexp "\n") g

let p2 file =
    In_channel.with_open_bin file In_channel.input_all
    |> split_g
    |> map (fun group -> process_g group |> compare_compartments )
    |> fold_left (fun a i -> a + (fst i)) 0

let () =
  p1 filename |> string_of_int |> print_endline;
  p2 filename |> string_of_int |> print_endline

