(* open Base *)
open Core

let alphas =
  String.split "a b c d e f g h i j k l m n o p q r s t u v w x y z" ~on:' '

let prioAlphas =
  List.mapi alphas ~f:(fun i alpha -> (alpha, i + 1))
  |> List.fold ~init:[] ~f:(fun acc (alpha, i) ->
         (alpha, i) :: (String.uppercase alpha, i + 26) :: acc)
  |> List.rev

(* let _ = *)
(*   In_channel.fold_lines In_channel.stdin ~init:0 ~f:(fun acc line -> *)
(*       let length = String.length line in *)
(*       let subS = String.sub line ~pos:0 ~len:(length / 2) in *)
(*       let subS2 = String.sub line ~pos:(length / 2) ~len:(length / 2) in *)
(*       let rec prioChar (i : int) (s : string) : string = *)
(*         if String.contains subS2 s.[i] then String.of_char s.[i] *)
(*         else prioChar (i + 1) s *)
(*       in *)
(*       let _, value = *)
(*         List.find_exn prioAlphas ~f:(fun (alpha, _) -> *)
(*             String.compare alpha (prioChar 0 subS) = 0) *)
(*       in *)
(*       acc + value) *)

let compute_three_lines x y z total =
  let rec find_common i =
    if String.contains y x.[i] && String.contains z x.[i] then x.[i]
    else find_common (i + 1)
  in
  let letter = String.of_char (find_common 0) in
  let _, num =
    List.find_exn prioAlphas ~f:(fun (alpha, _) ->
        String.compare alpha letter = 0)
  in
  (num + total, ("", "", ""))

let getInputsPartII =
  let total, (a, b, c) =
    In_channel.fold_lines In_channel.stdin
      ~init:(0, ("", "", ""))
      ~f:(fun (total, (x, y, z)) line ->
        if String.compare z "" = 0 then (total, (line, x, y))
        else
          let t, _ = compute_three_lines x y z total in
          (t, (line, "", "")))
  in
  let res, _ = compute_three_lines a b c total in
  res

(* let () = printf "Final ans: %i\n" getInputs *)
let () = printf "Part Two final ans: %i \n" getInputsPartII
