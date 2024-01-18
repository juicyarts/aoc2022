open List
open Str

let filename = Sys.argv.(1)

let parse_cargo cargo =
  let new_cargo = split (regexp "\n") cargo in
  let rows =
    init
      (length new_cargo - 1)
      (fun rI ->
        Re2.split (Re2.create_exn {|(\]\s|\s{4})|}) (nth new_cargo rI)
        |> map (fun i ->
               Re2.rewrite_exn (Re2.create_exn "\\[|\\]") ~template:"" i)
        |> map (function "" -> "x" | i -> i))
  in
  init
    (length (nth rows (length rows - 1)))
    (fun cI ->
      init (length rows) (fun rI ->
          match rI with
          | _ when length (nth rows rI) - 1 < cI -> "x"
          | _ -> nth (nth rows rI) cI)
      |> filter (( != ) "x")
      |> rev)

let parse_moves moves =
  split (regexp "\n") moves
  |> map (fun m ->
         Scanf.sscanf m "move %d from %d to %d" (fun amnt op np ->
             [ amnt; op - 1; np - 1 ]))

let cargo_moves (a : string list) (revert : bool) =
  parse_moves (nth a 1)
  |> fold_left
       (fun cargo moves ->
         mapi
           (fun i (row : string list) ->
             match i with
             | _ when i = nth moves 1 ->
                 filteri (fun i _ -> i < length row - nth moves 0) row
             | _ when i = nth moves 2 ->
                 let filtered =
                   filteri
                     (fun i _ ->
                       i >= length (nth cargo (nth moves 1)) - nth moves 0)
                     (nth cargo (nth moves 1))
                 in
                 if revert then row @ rev filtered else row @ filtered
             | _ -> row)
           cargo)
       (parse_cargo (nth a 0))
  |> fold_left
       (fun msg row ->
         match row with
         | _ when is_empty row != true -> msg ^ (rev row |> hd)
         | _ -> msg)
       ""

let p1 file =
  In_channel.with_open_bin file In_channel.input_all
  |> split (regexp "\n\n")
  |> cargo_moves

let () =
  p1 filename true |> print_string;
  print_endline "";
  p1 filename false |> print_string;
  print_endline ""
