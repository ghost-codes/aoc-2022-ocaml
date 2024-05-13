(* open Base *)
(* open Stdio *)
open Core

let replaceIfLess set value =
  let sorted = List.sort set ~compare:Int.compare in
  match sorted with
  | [] -> [ value; 0; 0 ]
  | [ x ] -> [ value; x; 0 ]
  | x :: tl -> Int.max value x :: tl

let getInputs =
  let ans =
    In_channel.fold_lines In_channel.stdin
      ~init:(0, [ 0; 0; 0 ])
      ~f:(fun acc line ->
        match line with
        | "" ->
            let x, y = acc in
            (0, replaceIfLess y x)
        | _ ->
            let num = int_of_string line in
            let x, tl = acc in
            (x + num, tl))
  in
  let x, y = ans in
  List.fold (replaceIfLess y x) ~init:0 ~f:(fun acc x -> acc + x)

let () = printf "Final Answer: %d" getInputs
