(* Problem 19.
  Rotate a list N places to the left. (medium)
  Examples:
  # rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
  - : string list = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]

  # rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2);;
  - : string list = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"]
*)

let rec split l n = 
  let rec do_split = fun l i ->
    match l with
      | [] -> ([], [])
      | a :: t ->
        let (list1, list2) = (do_split t (i + 1)) in
          if i >= n then (list1, a :: list2)
          else (a :: list1, list2)
  in
    do_split l 0
;; 

let rec rotate l k = 
  if k = 0 then l
  else
    if k > 0 then
      match l with
        | [] -> []
        | a :: t -> (rotate (t @ [a]) (k - 1))
    else
      let len = List.length l in
        let (left, right) = (split l (len + k)) in
          right @ left
;;

