(* Problem 1.
  Write a function
    last : 'a list -> 'a option that returns the last element of a list. *)

(* a quick dirty function that uses the List module *)
let last l = List.nth l ((List.length l) - 1) 

(* a slow recursive function that pattern matches *)
let rec last_slow = function 
  | [] -> None
  | [a] -> Some a
  | _ :: b -> last_slow b

(* our main program. Tests the slow recursive function. *)
let _ = print_int (
    match last_slow [1 ; 2 ; 3 ; 4 ; 5] with 
    | Some s -> s 
    | None -> -1 ) ; 
    print_string "\n" ; 
    match last_slow [0] with
    | Some s -> print_int s ; print_string "\n"
    | None -> print_string "none is found.\n" ; 
   print_string "\n"
