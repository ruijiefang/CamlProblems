(* Problem 19.
  Rotate a list N places to the left. (medium)
  Examples:
  # rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
  - : string list = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]

  # rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2);;
  - : string list = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"]
*)

let rec rotate l k = 
  if k = 0 then l
  else
    match l with
      | [] -> []
      | a :: t ->
        if k > 0 then (rotate (t @ [a]) (k - 1))
        else (rotate )
    else
      let j = -k in
        
;;

