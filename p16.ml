(* Problem 16. Drop every N'th element from a list. (medium).
 # drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
- : string list = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]
*)

let rec drop l n = 
  let rec do_drop = fun l k ->
    match k with 
      | 1 -> 
        match l with
          | [] -> ([], [])
          | a :: t -> t
      | j -> 
        match l with
          | [] -> []
          | a :: t -> a :: (do_drop l (k - 1))
  in
    match (do_drop l n) with
      | [] -> []
      | a :: t -> 
