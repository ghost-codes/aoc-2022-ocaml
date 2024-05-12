(* open Base *)
(* open Stdio *)
open Core

let getInputs =
  let ans =
    In_channel.fold_lines In_channel.stdin ~init:(0, 0) ~f:(fun acc line ->
        match line with
        | "" ->
            let x, y = acc in
            (0, Int.max x y)
        | _ ->
            let num = int_of_string line in
            let x, tl = acc in
            (x + num, tl))
  in
  let x, y = ans in
  Int.max x y

let () = printf "Final Answer: %d" getInputs
