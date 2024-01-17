open List
open Str

let filename = Sys.argv.(1)

let comp_ranges a b = match a with
  | _ when ((nth a 0) >= (nth b 0) && (nth a 1) <= (nth b 1)) || ((nth b 0) >= (nth a 0) && (nth b 1) <= (nth a 1))  -> 1
  | _ -> 0

let p1 file =
    In_channel.with_open_bin file In_channel.input_all
    |> split (regexp "\n")
    |> map (fun a -> (split (regexp ",") a |> map (fun b -> (split (regexp "-") b) |> map (fun c -> int_of_string c))))
    |> map (fun a -> comp_ranges (nth a 0) (nth a 1))
    |> fold_left (+) 0

let comp_ranges_p2 a b = match a with
  | _ when ((nth b 0) > (nth a 1) || (nth b 1 < nth a 0)) || ((nth a 0) > (nth b 1) || (nth a 1 < nth b 0))  -> 0
  | _ ->  1

let p2 file =
    In_channel.with_open_bin file In_channel.input_all
    |> split (regexp "\n")
    |> map (fun a -> (split (regexp ",") a |> map (fun b -> (split (regexp "-") b) |> map int_of_string)))
    |> map (fun a -> comp_ranges_p2 (nth a 0) (nth a 1))
    |> fold_left (+) 0

let () =
  let res = p1 filename in
  print_endline (string_of_int res);
  let res2 = p2 filename in
  print_endline (string_of_int res2)
