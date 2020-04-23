(* Problem 20.
 Remove the K'th element from a list. (easy)
  The first element of the list is numbered 0, 
  the second 1,...

  Example:
  # remove_at 1 ["a";"b";"c";"d"];;
  - : string list = ["a"; "c"; "d"]
*)

let rec remove_at k l =
  match l with
    | [] -> []
    | a :: t -> 
      if k == 0 then t
      else a :: (remove_at (k - 1) t)
