(* open Base *)
open Core

let trim_all_braces line =
  String.substr_replace_all line ~pattern:"[" ~with_:""
  |> String.substr_replace_all ~pattern:"]" ~with_:""

(* let rec process_brick line arr num = *)
(*   match line with *)
(*   | [] -> arr *)
(*   | [ brick ] -> *)
(*       let brick' = *)
(*         String.substr_replace_all ~pattern:" " ~with_:"" brick *)
(*         |> trim_all_braces *)
(*       in *)
(*       if not (String.length brick' = 0) then arr.(num) <- brick' :: arr.(num); *)
(*       process_brick [] arr num *)
(*   | brick :: tl -> *)
(*       let brick' = *)
(*         String.substr_replace_all ~pattern:" " ~with_:"" brick *)
(*         |> trim_all_braces *)
(*       in *)
(*       if not (String.length brick' = 0) then arr.(num) <- brick' :: arr.(num); *)
(*       process_brick tl arr num *)

let store_all_bricks arr line num =
  let len = num - 1 in
  for i = 0 to len do
    let item = String.sub line ~pos:(i * 4) ~len:3 in
    let item' = String.strip item in
    let item'' = trim_all_braces item' in
    if not (String.length item'' = 0) then arr.(i) <- arr.(i) @ (item'' :: [])
    else ()
  done;

  arr

let rec proccess_raw_bricks arr bricks num =
  match bricks with
  | [] -> arr
  | [ x ] -> store_all_bricks arr x num
  | hd :: tl -> proccess_raw_bricks (store_all_bricks arr hd num) tl num

let rec process_move ~count ~from ~to_ bricks =
  if count = 0 then bricks
  else
    match bricks.(from) with
    | [] -> bricks
    | [ x ] ->
        bricks.(to_) <- x :: bricks.(to_);
        bricks.(from) <- [];

        process_move ~count:(count - 1) ~from ~to_ bricks
    | x :: tl ->
        bricks.(to_) <- x :: bricks.(to_);
        bricks.(from) <- tl;
        process_move ~count:(count - 1) ~from ~to_ bricks

let rec process_moves l bricks =
  match l with
  | [] -> bricks
  | [ x ] -> process_move ~count:x.(0) ~from:(x.(1) - 1) ~to_:(x.(2) - 1) bricks
  | x :: tl ->
      let br =
        process_move ~count:x.(0) ~from:(x.(1) - 1) ~to_:(x.(2) - 1) bricks
      in

      process_moves tl br

let check_if_number_line line =
  let x = String.substr_replace_all ~pattern:" " ~with_:"" line in
  let x' = String.strip x in
  Int.of_string_opt (String.of_char x'.[String.length x - 1])
(* Int.of_string_opt x *)
(* if String.contains x 'n' then Some 6 else None *)

let process_outputs res num =
  let x = ref " " in
  for i = 0 to num do
    match res.(i) with
    | [] -> x := " " ^ !x
    | [ y ] -> x := y ^ !x
    | hd :: _ -> x := hd ^ !x
  done;
  !x

let getInputs =
  let _, moves, bricks, length =
    In_channel.fold_lines In_channel.stdin ~init:(false, [], [], 0)
      ~f:(fun acc line ->
        if not (String.length line = 0) then
          match acc with
          | x, y, z, w when not x -> (
              let num = check_if_number_line line in

              match num with
              | Some a -> (true, y, z, a)
              | None -> (false, y, line :: z, w))
          | x, moves, z, w ->
              let pMove =
                String.substr_replace_all ~pattern:"move " ~with_:"" line
                |> String.substr_replace_all ~pattern:"from " ~with_:""
                |> String.substr_replace_all ~pattern:"to " ~with_:""
                |> String.strip |> String.split ~on:' '
                |> List.map ~f:(fun x -> int_of_string x)
                |> Array.of_list
              in
              (x, pMove :: moves, z, w)
        else acc)
  in

  let pBricks =
    proccess_raw_bricks (Array.create ~len:length []) bricks length
  in
  let pBricks' = Array.map pBricks ~f:List.rev in
  print_endline "Before Moves";
  List.iter
    ~f:(fun x ->
      List.iter ~f:(printf "%s") x;
      printf "%s\n" "")
    (List.of_array pBricks');

  let res = process_moves moves pBricks' in
  print_endline "After Moves";
  List.iter
    ~f:(fun x ->
      List.iter ~f:(printf "%s") x;
      printf "%s\n" "")
    (List.of_array res);
  (* let res' = Array.map res ~f:List.rev in *)
  let a = process_outputs res (length - 1) in
  (* length *) String.rev a

let () = printf "Final ans:%s" getInputs
