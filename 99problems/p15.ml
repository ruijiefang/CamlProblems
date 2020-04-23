(* Problem 15. Replicate the elements of a list a given number of times. (medium)
 Example:
 # replicate ["a";"b";"c"] 3;;
- : string list = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
*)

open Printf

let rec replicate = fun l n ->
  (* suffices for our purposes, 
    although the compiler (reasonably)
    warns of the case of the empty list,
    which doesn't matter (and cannot be solved)
    since we can't typecheck if a list is infinite.
    
    the other alternative is to create a recursive
    type, but we don't want to do that for brevity.
    *)
  let rec npeek = fun s n ->
    match n with 
      | 0 -> []
      | k -> let a :: _ = s in
        a :: (npeek s (n  - 1)) in
    match l with
      | [] -> []
      | a :: t -> 
        let rec s = a :: s in
          (npeek s n) @ (replicate t n)
;;
let rec print_str_list = function
  | [] -> ()
  | [a] -> printf "%s]\n" a
  | a :: t -> printf "%s, " a ; 
    print_str_list t
;;
let () = 
  printf "[";
  print_str_list ["a";"b";"c"];
  printf "[" ;
  print_str_list (replicate ["a";"b";"c"] 3)

