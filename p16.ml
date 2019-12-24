(* Problem 16. Drop every N'th element from a list. (medium).
 # drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
- : string list = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]
*)

open Printf;;

(* drop l n
  drops every n-th element from list l *)
let rec drop l n = 
  let rec do_drop = fun l k n ->
    if k == n then
      match l with
        | [] -> []
        | a :: t -> do_drop t 1 n
    else
      match l with
        | [] -> []
        | a :: t -> 
          a :: (do_drop t (k + 1) n)
  in
    do_drop l 1 n
;;




