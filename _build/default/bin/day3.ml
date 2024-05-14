open Base
open Core

let alphas =
  String.split "a b c d e f g h i j k l m n o p q r s t u v w x y z" ~on:' '

let prioAlphas =
  List.mapi alphas ~f:(fun i alpha -> (alpha, i + 1))
  |> List.fold ~init:[] ~f:(fun acc (alpha, i) ->
         (alpha, i) :: (String.uppercase alpha, i + 26) :: acc)
  |> List.rev

let getInputs =
  In_channel.fold_lines In_channel.stdin ~init:0 ~f:(fun acc line ->
      let length = String.length line in
      let subS = String.sub line ~pos:0 ~len:(length / 2) in
      let subS2 = String.sub line ~pos:(length / 2) ~len:(length / 2) in
      let rec prioChar (i : int) (s : string) : string =
        if String.contains subS2 s.[i] then String.of_char s.[i]
        else prioChar (i + 1) s
      in
      let _, value =
        List.find_exn prioAlphas ~f:(fun (alpha, _) ->
            String.compare alpha (prioChar 0 subS) = 0)
      in
      acc + value)

let () = printf "Final ans: %i\n" getInputs
