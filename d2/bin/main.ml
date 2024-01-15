open List

let filename = Sys.argv.(1)

let s_to_shape s =
  if s = "A" || s = "X" then 1 else if s = "B" || s = "Y" then 2 else if s = "C" || s = "Z" then 3 else 0

let get_score a i =
  if a = 0 then i else if i = a then 3 + i else if (a = 1 && i !=2) || (a = 2 && i != 3) || (a = 3 && i != 1) then 0 + i else 6+i

let p1 file =
    In_channel.with_open_bin file In_channel.input_all
    |> Str.split (Str.regexp "\n")
    |> List.map (fun f -> Str.split (Str.regexp " ") f |> map s_to_shape
      |> fold_left get_score 0)
    |> fold_left (+) 0

let () =
    p1 filename |> string_of_int |> print_endline;
