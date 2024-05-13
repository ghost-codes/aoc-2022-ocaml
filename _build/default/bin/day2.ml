(* open Base *)
open Core

(* let scoreMatches = *)
(*   [ *)
(*     ("X", [ ("A", 4); ("B", 1); ("C", 7) ]); *)
(*     ("Y", [ ("A", 8); ("B", 5); ("C", 2) ]); *)
(*     ("Z", [ ("A", 3); ("B", 9); ("C", 6) ]); *)
(*   ] *)

(* let outcomeNumbers = *)
(*   [ *)
(*     ("X", [ ("A", 1, "Y"); ("B", 2, "Z"); ("C", 3, "X") ]); *)
(*     ("Y", [ ("A", 4, "X"); ("B", 5, "Y"); ("C", 6, "Z") ]); *)
(*     ("Z", [ ("A", 7, "Z"); ("B", 8, "X"); ("C", 9, "Y") ]); *)
(*   ] *)

let outScore = [ ("X", 0); ("Y", 3); ("Z", 6) ]

let myMove =
  [
    ("A", [ ("X", "C"); ("Y", "A"); ("Z", "B") ]);
    ("B", [ ("X", "A"); ("Y", "B"); ("Z", "C") ]);
    ("C", [ ("X", "B"); ("Y", "C"); ("Z", "A") ]);
  ]

let moveScore = [ ("A", 1); ("B", 2); ("C", 3) ]

(* let calculateScore (opp : string) (move : string) : int = *)
(*   let _, possibleScore = *)
(*     List.find_exn scoreMatches ~f:(fun x -> *)
(*         let m, _ = x in *)
(*         String.compare move m = 0) *)
(*   in *)
(*   let map = *)
(*     List.find_exn possibleScore ~f:(fun x -> *)
(*         let o, _ = x in *)
(*         String.compare o opp = 0) *)
(*   in *)
(*   let _, res = map in *)
(*   res *)

let calculateScoreGivenOutcome opp out =
  let _, outs =
    List.find_exn myMove ~f:(fun (m, _) -> String.compare opp m = 0)
  in
  let _, my_move =
    List.find_exn outs ~f:(fun (m, _) -> String.compare m out = 0)
  in
  let _, myMoveScore =
    List.find_exn moveScore ~f:(fun (m, _) -> String.compare my_move m = 0)
  in
  let _, myOutScore =
    List.find_exn outScore ~f:(fun (m, _) -> String.compare out m = 0)
  in
  myOutScore + myMoveScore

let getInputs =
  In_channel.fold_lines In_channel.stdin ~init:0 ~f:(fun acc line ->
      let values = String.split line ~on:' ' in
      match values with
      | [] -> acc
      | [ _ ] -> acc
      | x :: y :: _ -> acc + calculateScoreGivenOutcome x y)

let () = printf "Final ans: %i" getInputs
