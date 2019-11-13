(* Problem 14. Duplicate the elements of a list. (easy)
 Example:
 # duplicate ["a";"b";"c";"c";"d"];;
- : string list = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]
*)

open Printf

let rec duplicate = function
  | [] -> []
  | a :: t -> a :: a :: (duplicate t)

let rec print_str_list = function
  | [] -> ()
  | [a] -> printf "%s]\n" a 
  | a :: t -> printf "%s, " a ; 
    print_str_list t

let () = 
  printf "[" ;
  print_str_list ["a";"b";"c";"c";"d"] ;
  printf "[" ;
  print_str_list (duplicate ["a";"b";"c";"c";"d"])
