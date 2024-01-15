open List
open Str

let filename = Sys.argv.(1)

let p1 file =
    let contents = In_channel.with_open_bin file In_channel.input_all in
    fold_left (fun (a, i) b -> if i < 1 then (a+b, i+1) else (a, i+1)) (0,0)
    (rev (sort Int.compare (map (fun s ->
        fold_left (+) 0 (
            map (int_of_string) (split (regexp "\n") s))
        ) (split (regexp "\n\n") contents) 
    )))

let p2 file =
    let contents = In_channel.with_open_bin file In_channel.input_all in
    fold_left (fun (a, i) b -> if i < 3 then (a+b, i+1) else (a, i+1)) (0,0)
    (rev (sort Int.compare (map (fun s ->
        fold_left (+) 0 (
            map (int_of_string) (split (regexp "\n") s))
        ) (split (regexp "\n\n") contents) 
    )))

let () =
    let (a, _) = p1 filename in
    print_endline (string_of_int a);
    let (b, _) = p2 filename in
    print_endline (string_of_int b);
