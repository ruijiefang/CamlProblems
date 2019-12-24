(* Problem 18.
 Given two indices, i and k, the slice is the 
 list containing the elements between the i'th
 and k'th element of the original list (both 
 limits included). Start counting the elements 
 with 0 (this is the way the List module numbers
 elements).

 Example:
  # slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6;;
  - : string list = ["c"; "d"; "e"; "f"; "g"]
*)

let rec slice l i k =
  let rec do_slice = fun l j ->
    match l with
      | [] -> []
      | a :: t ->
        if j >= i && j <= k then
          a :: (do_slice t (j + 1))
        else 
          (do_slice t (j + 1))
  in
    do_slice l 0
;;

