open Base
open Core

let scoreMatches =
  [
    ("X", [ ("A", 4); ("B", 1); ("C", 7) ]);
    ("Y", [ ("A", 8); ("B", 5); ("C", 2) ]);
    ("Z", [ ("A", 3); ("B", 9); ("C", 6) ]);
  ]

let calculateScore (opp : string) (move : string) : int =
  let _, possibleScore =
    List.find_exn scoreMatches ~f:(fun x ->
        let m, _ = x in
        String.compare move m = 0)
  in
  let map =
    List.find_exn possibleScore ~f:(fun x ->
        let o, _ = x in
        String.compare o opp = 0)
  in
  let _, res = map in
  res

let getInputs =
  In_channel.fold_lines In_channel.stdin ~init:0 ~f:(fun acc line ->
      let values = String.split line ~on:' ' in
      match values with
      | [] -> acc
      | [ _ ] -> acc
      | x :: y :: _ -> acc + calculateScore x y)

let () = printf "Final ans: %i" getInputs
