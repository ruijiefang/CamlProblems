(* Problem 8. Eliminate consecutive duplicates of list elements. (medium)
 Example:
 # compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
- : string list = ["a"; "b"; "c"; "a"; "d"; "e"] *)

open Printf

let compress l = 
  let rec scan = function
    | ([], b) -> []
    | (a :: t, b) ->
      if a = b then scan (t, b) 
      else a :: (scan (t,a))
  in match l with
    | [] -> []
    | [a] -> [a]
    | a :: t -> a :: (scan (t, a)) 

let () = List.iter (printf "%s ") (compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]) ;
    print_string "\n"
