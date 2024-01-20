open List
open Str

let filename = Sys.argv.(1)

let rec transpose list =
  match list with
  | [] -> []
  | [] :: xss -> transpose xss
  | (x :: xs) :: xss -> List.((x :: map hd xss) :: transpose (xs :: map tl xss))

let print_map (m : int list list) v =
  iteri
    (fun i (row : int list) ->
      print_endline "";
      iteri
        (fun j (col : int) ->
          if Hashtbl.mem v (string_of_int i ^ "|" ^ string_of_int j) then
            Spectrum.Simple.printf "@{<green>%s@}" (string_of_int col)
          else print_int col)
        row)
    m

let p1 file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  let grid =
    split (regexp "\n") contents
    |> map (fun i -> split (regexp "") i |> map int_of_string)
  in
  print_endline "\nORIGINAL--";
  let visited = Hashtbl.create (length (nth grid 1) * length grid) in
  print_map grid visited;
  print_endline "\nTRANSPOSED--";
  print_map (transpose grid) visited;
  print_endline "";
  let horizontal =
    mapi
      (fun rI row ->
        let _, cnt, _ =
          fold_left
            (fun (cI, acc, h) i ->
              match i with
              | _ when h = 9 -> (cI+1, acc, h)
              | _ when i > h || cI = 0 || rI = 0 || cI = (length row -1) || rI = (length grid -1)  ->
                  if
                    Hashtbl.mem visited
                      (string_of_int rI ^ "|" ^ string_of_int cI)
                  then ()
                  else
                    Hashtbl.add visited
                      (string_of_int rI ^ "|" ^ string_of_int cI)
                      1;
                  (cI + 1, acc + 1, i)
              | _ -> (cI + 1, acc, h))
            (0, 0, 0) row
        in
        let _, cnt1, _ =
          fold_left
            (fun (cI, acc, h) i ->
              match i with
              | _ when h = 9 -> (cI+1, acc, h)
              | _ when i > h || cI = 0 || rI = 0 || cI = (length row -1) || rI = (length grid -1)  ->
                  if
                    Hashtbl.mem visited
                      (string_of_int rI ^ "|"
                      ^ string_of_int (length row - 1 - cI))
                  then ()
                  else
                    Hashtbl.add visited
                      (string_of_int rI ^ "|"
                      ^ string_of_int (length row - 1 - cI))
                      1;
                  (cI + 1, acc + 1, i)
              | _ -> (cI + 1, acc, h))
            (0, 0, 0) (row |> rev)
        in
        (cnt + cnt1, 0))
      grid
  in
  let vertical =
    mapi
      (fun rI row ->
        let _, cnt, _ =
          fold_left
            (fun (cI, acc, h) i ->
              match i with
              | _ when h = 9 -> (cI+1, acc, h)
              | _ when i > h || cI = 0 || rI = 0 || cI = (length row -1) || rI = (length grid -1)  ->
                  if
                    Hashtbl.mem visited
                      (string_of_int cI ^ "|" ^ string_of_int rI)
                  then ()
                  else
                    Hashtbl.add visited
                      (string_of_int cI ^ "|" ^ string_of_int rI)
                      1;
                  (cI + 1, acc + 1, i)
              | _ -> (cI + 1, acc, h))
            (0, 0, 0) row
        in
        let _, cnt1, _ =
          fold_left
            (fun (cI, acc, h) i ->
              match i with
              | _ when h = 9 -> (cI+1, acc, h)
              | _ when i > h || cI = 0 || rI = 0 || cI = (length row -1) || rI = (length grid -1)  ->
                  if
                    Hashtbl.mem visited
                      (string_of_int (length row - 1 - rI)
                      ^ "|"
                      ^ string_of_int (length grid - 1 - cI))
                  then ()
                  else
                    Hashtbl.add visited
                      (string_of_int (length row - 1 - rI)
                      ^ "|"
                      ^ string_of_int (length grid - 1 - cI))
                      1;
                  (cI + 1, acc + 1, i)
              | _ -> (cI + 1, acc, h))
            (0, 0, 0) (row |> rev)
        in
        (cnt + cnt1, 0))
      (transpose grid)
  in
  Hashtbl.iter (fun x y -> Printf.printf "%s -> %d\n" x y) visited;
  print_endline "AFTER--";
  print_map grid visited;
  print_endline "";
  print_endline (string_of_int (Hashtbl.length visited));
  List.fold_left (fun acc (a, _) -> acc + a) 0 (horizontal @ vertical)

let () =
  let a : int = p1 filename in
  print_int a;
  print_endline ""
