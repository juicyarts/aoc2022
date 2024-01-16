open List
open Str

let filename = Sys.argv.(1)

let win_list = [[3; 1; 2]; [1; 2; 3]; [2; 3; 1]]

let score a i = match a with
  | 5 -> i
  | _ when i = a -> 3+i+1
  | _ when (nth (nth win_list a) 2) == i+1 -> 6+i+1
  | _ -> i+1

let shape s = match s with
  | "B" | "Y" -> 1
  | "C" | "Z" -> 2
  | _ -> 0

let p1 file =
    In_channel.with_open_bin file In_channel.input_all
    |> split (regexp "\n")
    |> map (fun f -> split (regexp " ") f |> map shape |> fold_left score 5)
    |> fold_left (+) 0

 (*p2*)
let points b = match b with
  | "Y" -> 3
  | "Z" -> 6
  | _ -> 0

let score_p2 a i = match a with
  | 5 -> (shape i)
  | _ -> ((nth (nth win_list a) (shape i)) + (points i))

let p2 file =
    In_channel.with_open_bin file In_channel.input_all
    |> split (regexp "\n")
    |> map (fun f -> split (regexp " ") f |> fold_left score_p2 5)
    |> fold_left (+) 0

let () =
    p1 filename |> string_of_int |> print_endline;
    print_endline "";
    p2 filename |> string_of_int |> print_endline;
