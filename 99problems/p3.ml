(* Problem 3. Find the k-th element of a list. *)
let rec kth k = function 
  | [] -> None
  | h :: t -> if k = 1 then Some h else kth (k - 1) t

let _ = 
  print_int (match kth 3 [1;2;3;4;5;6;7;8] with
      | Some n -> n 
      | None -> -1);
  print_string "\n"
