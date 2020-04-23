(* Problem 2. Find the (secont to last, last) elements in the list. *)
open Printf 
let rec last_two = function 
  | [] | [_] -> None
  | [a; b] -> Some (a, b)
  | _ :: t -> last_two t

let () = 
  match (last_two [1;2;3;4;5]) with
    | None -> printf "No (last, penultimate) found\n"
    | Some (a, b) -> printf "Yes (%d, %d)\n" a b 
  ; ()
