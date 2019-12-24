(* Problem 22.
  Create a list containing all integers within a given range. (easy)
  If first argument is greater than second, produce a list in decreasing order.

  Examples.
  # range 4 9;;
  - : int list = [4; 5; 6; 7; 8; 9]
  # range 9 4;;
  - : int list = [9; 8; 7; 6; 5; 4]
*)

let rec range a b = 
  if a == b then [a]
  else
    if a < b then (range a (b - 1)) @ [b]
    else (* a > b *) 
      a :: (range (a - 1) b)
