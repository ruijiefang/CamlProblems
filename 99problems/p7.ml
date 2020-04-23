
(* Problem 7. Flatten a nested list structure. *)

open Printf

(* There is no nested list type in OCaml, so we need to define one
     first. A node of a nested list is either an element, or a list of
     nodes. *)
type 'a node =
  | One of 'a 
  | Many of 'a node list;;
(* type 'a node = One of 'a | Many of 'a node list *)

let rec flatten = function 
  | [] -> []
  | One h :: t -> [h] @ (flatten t) 
  | Many h :: t -> (flatten h) @ (flatten t) 

let () = 
  List.iter (printf "%s ") (flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ]) ; 
  printf "\n"
