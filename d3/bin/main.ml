open List
open Str
let filename = Sys.argv.(1)

let alphabet =
  let lowercase = List.init 26 (fun i -> (i+1, Char.chr (i + Char.code 'a'))) in
  let uppercase = List.init 26 (fun i -> ((i+26)+1, Char.chr ((i) + Char.code 'A'))) in
  lowercase @ uppercase

let split_sack a = [(string_before a (String.length(a) / 2)); (string_after a (String.length(a) / 2))]

let compare_compartments (a: string list) =
  List.init (String.length (nth a 0))  (String.get (nth a 0))
  |> fold_left (fun x c -> if (String.contains (nth a 1) c) then ((List.find (fun (_,a) -> a = c ) alphabet)) else x ) (nth alphabet 0)

let p1 file =
    In_channel.with_open_bin file In_channel.input_all
    |> split (regexp "\n")
    |> map (fun line -> (split_sack line) |> compare_compartments)
    |> fold_left (fun a i -> a + (fst i)) 0

let compare_compartments_p2 (a: string list) =
  let bar = fold_left (fun (x:string) (c: string) -> if (String.length c) > (String.length x) then c else x) (nth a 0) a in
  List.init (String.length bar)  (String.get bar)
  |> fold_left (fun x c -> if (String.contains (nth a 0) c) && (String.contains (nth a 1) c) && (String.contains (nth a 2) c) then ((List.find (fun (_,a) -> a = c ) alphabet)) else x ) (nth alphabet 0)

let p2 file =
    In_channel.with_open_bin file In_channel.input_all
    |> Re2.find_all_exn (Re2.create_exn "(?:.+\n?){3}")
    |> map (fun group -> (split (regexp "\n") group) |> compare_compartments_p2 )
    |> fold_left (fun a i -> a + (fst i)) 0

let () =
  p1 filename |> string_of_int |> print_endline;
  p2 filename |> string_of_int |> print_endline;
