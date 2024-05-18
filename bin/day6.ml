open Core

let file_name =
  "/Users/hopedorkenoo/Documents/personal/ocaml/adventOfCode/day1/bin/a.txt"

let check_condition str =
  let len = String.length str in
  let arr = ref [] in
  for i = 0 to len - 1 do
    arr := List.append !arr [ String.of_char str.[i] ]
  done;
  let newLen =
    List.dedup_and_sort !arr ~compare:String.compare |> List.length
  in
  newLen = len

let rec compute_conservative_chars num line =
  if num > String.length line - 14 then 0
  else
    let sub = String.sub line ~pos:num ~len:14 in
    let is_corr = check_condition sub in
    if is_corr then num + 14 else compute_conservative_chars (num + 1) line

let get_inputs =
  let ic = In_channel.create file_name in
  let res =
    try
      let line = In_channel.input_line ic in
      line
    with e ->
      In_channel.close ic;
      raise e
  in
  match res with None -> 2 | Some x -> compute_conservative_chars 0 x

(* match line  | Some x -> x *)

let () = printf "\n\n%i\n" get_inputs
