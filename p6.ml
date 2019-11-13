(* Problem 6. Find out whether a list is palindrome. *)
open Printf

let is_pal l =
  let rec rev = function
    | [] -> []
    | a :: t -> (rev t) @ [a] in
    l = (rev l)

let () = 
  let print_boolean b =
    printf "%B\n" b
  in
    print_boolean (is_pal [1;2;3;4]) ;
    print_boolean (is_pal [1;2;3;2;1])
