open List
open Str

let filename = Sys.argv.(1)

let p1 file =
    In_channel.with_open_bin file In_channel.input_all
    |> split (regexp "\n\n")
    |> map (fun s -> split (regexp "\n") s |> map int_of_string |> fold_left (+) 0)
    |> sort Int.compare |> rev
    |> hd

let p2 file =
    In_channel.with_open_bin file In_channel.input_all 
    |> split (regexp "\n\n")
    |> map (fun s -> split (regexp "\n") s |> map int_of_string |> fold_left (+) 0)
    |> sort Int.compare |> rev
    |> fold_left (fun (a, i) b -> if i < 3 then (a+b, i+1) else (a, i+1)) (0,0)

let () =
    p1 filename |> string_of_int |> print_endline;
    let (b, _) = p2 filename in
    print_endline (string_of_int b);
