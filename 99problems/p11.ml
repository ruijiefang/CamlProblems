(* Problem 11. Modified run-length encoding. (easy)
  Modify the result of the previous problem 
  in such a way that if an element has no 
  duplicates it is simply copied into the 
  result list. Only elements with 
  duplicates are 
  transferred as (N E) lists.

  Since OCaml lists are homogeneous, 
  one needs to define a 
  type to hold both single elements and 
  sub-lists.

  # type 'a rle =
    | One of 'a
    | Many of int * 'a;;

  type 'a rle = One of 'a | Many of int * 'a

  Example:
  # encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
- : string rle list =
  [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
  Many (4, "e")]
*)

open Printf 

type 'a rle = 
  | One of 'a
  | Many of int * 'a 

let rec encode = function
  | [] -> []
  | [a] -> [One a]
  | a :: (b :: _ as t) ->
    if a = b then
      match (encode t) with
        | [] -> [Many (2, a)]
        | Many (cnt, elem) :: rest -> 
            Many (cnt + 1, elem) :: rest
        | One elem :: rest ->
            Many (2, elem) :: rest
    else
      One a :: (encode t)
