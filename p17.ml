(* Problem 17.
  Split a list into two parts;
  the length of the first part is
  given. (easy)
  
  If the length of the first part
  is longer than the entire list,
  then the first part is the list 
  and the second part is empty.

  Examples:
  # split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
  - : string list * string list =
  (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])

  # split ["a";"b";"c";"d"] 5;;
  - : string list * string list = (["a"; "b"; "c"; "d"], [])

*)

open Printf ;;

let rec split l n = 
  let rec do_split = fun l i ->
    if i >= n then
      match l with 
        | [] -> ([], [])
        | a :: t -> 
          match (do_split t (i + 1)) with
            | (list1, list2) ->
              (list1, a :: list2)
    else
      match l with
        | [] -> ([], [])
        | a :: t ->
          match (do_split t (i + 1)) with
            | (list1, list2) ->
              (a :: list1, list2)
  in
    do_split l 0
;; 
