(* Problem 5. Reverse a list. *)

open Printf

(* returns a reversed list *) 
let rec rev = function
  | [] -> []
  | a :: t -> (rev t) @ [a] 

(* prints the list as input *)
let rec print_list = function
  | [] -> printf "\n" ; ()
  | a :: t -> 
    printf "%d " a ; print_list t

(* a list *)
let l = [1;2;3;4;5]

(* the driver *)
let () = 
  printf "before reverse: " ;
  print_list l ;
  printf "after reverse: " ;
  print_list (rev l)

 
