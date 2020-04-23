(* p10. run-length encoding of a list (easy).
example:
# encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
- : (int * string) list =
[(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
*)

open Printf 

(* returns a list of (int * string) elements. *)
let rec encode = function
  | [] -> []
  | [a] -> [(1, a)]
  | a :: (b :: _ as t) ->
    if a = b then
      match encode t with
        | [] -> [(2, a)]
        | (count, element) :: tl -> 
          (count + 1, element) :: tl
    else 
      (1, a) :: (encode t)


let () = 
  let rec print_tpl_int_string = function
    | [] -> ()
    | [(a,b)] -> printf "(%d,%s)];\n" a b
    | (a, b) :: t -> printf "(%d,%s)," a b ; 
      print_tpl_int_string t 
  in 
  printf "[" ;
  print_tpl_int_string (encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"])
