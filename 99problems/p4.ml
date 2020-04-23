(* Problem 4. Find the number of elements of a list. *)

let rec len = function 
  | [] -> 0
  | _ :: b -> 1 + (len b)

let () = print_int (len [1;2;3;4;5]) ; print_string "\n"
