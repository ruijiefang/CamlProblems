(* Problem 9. Pack consecutive duplicates of list elements into sublists. (medium) *)


open Printf

(* Example:
 pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;
- : string list list =
[["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
 ["e"; "e"; "e"; "e"]]
*)

(* pack from left to right ; the return result is the 
  partial result starting from the list fed into the 
  next level of recursion.

  at each level, pattern match the recursion result 
  (unfortunately, we can't use a single let statement
   because ocaml warns of the `[]` case; though it 
   won't happen.)

  if we're in the middle of a consecutive sequence
  of identical elements, unpack the returned result
  and add ourselves as the head of the first element
  in the list.

  otherwise, append ourselves as the first element
  to the returned list. *)
let rec pack = function
  | [] -> []
  | [a] -> [[a]]
  | a :: (b :: _ as t) ->
    if a = b then 
      match pack t with
        | [] -> [[a;b]]
        | head :: tail ->
          (a :: head) :: tail
    else
      let l = pack t in
        [a] :: l

(* prints a list of strings *)
let rec pprint_list = function
  | [] -> ()
  | [a] -> printf "%s];\n" a
  | a :: t -> printf "%s, " a; pprint_list t

(* prints a list of lists of strings. *)
let rec pprint_listlist = function
  | [] -> ()
  | [a] -> printf "[" ; pprint_list a ; ()
  | a :: t -> printf "[" ; pprint_list a ; pprint_listlist t

(* the driver code. calls pprint_listlsit with the result from pack. *)
let () = 
  let l = pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"] in
    pprint_listlist l ;

