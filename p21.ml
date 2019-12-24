(* 21. Insert an element at a given position into a list. (easy)
  Start counting list elements with 0. 
  If the position is larger or equal to the 
  length of the list, insert the 
  element at the end. (The behavior is 
    unspecified if the position is negative.)

  Examples:
  # insert_at "alfa" 1 ["a";"b";"c";"d"];;
  - : string list = ["a"; "alfa"; "b"; "c"; "d"]
  # insert_at "alfa" 3 ["a";"b";"c";"d"];;
  - : string list = ["a"; "b"; "c"; "alfa"; "d"]
  # insert_at "alfa" 4 ["a";"b";"c";"d"];;
  - : string list = ["a"; "b"; "c"; "d"; "alfa"]
*)
let rec insert_at e k l =
  match l with
    | [] -> [e]
    | a :: t ->
      if k == 0 then [e ; a] @ t
      else a :: (insert_at e (k - 1) t)
