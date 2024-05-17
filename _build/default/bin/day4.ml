open Core

(* let find_total_overlap x y = *)
(*   (x.(0) >= y.(0) && x.(1) <= y.(1)) || (y.(0) >= x.(0) && y.(1) <= x.(1)) *)

(* 2-4 3-6 *)
let find_any_overlap x y =
  if x.(0) >= y.(0) then y.(1) >= x.(0) else x.(1) >= y.(0)

let getInputs =
  In_channel.fold_lines In_channel.stdin ~init:0 ~f:(fun acc line ->
      let plots = String.split line ~on:',' in
      let values =
        List.map plots ~f:(fun x ->
            List.map (String.split ~on:'-' x) ~f:(fun y -> int_of_string y))
      in
      match values with
      | [] -> acc
      | [ _ ] -> acc
      | hd :: tl :: _ ->
          if find_any_overlap (Array.of_list hd) (Array.of_list tl) then acc + 1
          else acc)

let () = printf "Final answer: %i" getInputs
