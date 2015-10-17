(* Practice problems for OCaml *)

(* Practice: Drop every n-th element *)

(* OCaml is left to right, for line 11
  a::temp b n (m-1) is right;
  a::temp b n m-1 becomes (a::temp b n m)-1, and is wrong
*)

let rec temp list n m = 
  match list with 
    | a :: b -> if m = 1 
                  then temp b n n 
                  else a::temp b n (m-1)
    | [] -> [];; (* Returning None here would be wrong, as None is a different type; Some (something) appears to be the same type as None *)

let drop list n = 
  temp list n n;;

let test1 = drop [1;2;3;4] 1;;
let test2 = drop [1;2;3;4] 2;;
let test3 = drop [1;2;3;4] 3;;

(* Moral:
   Think in terms of what a function takes, and what something returns; 
   Find how the idea of local variable works; 
   Think in terms of recursion; *)


(* Practice: see if the first element of array matches *)

let match_first list a =
  match list with
    | h::t -> if a = h then true else false
    | [] -> false

(* Doing | a::t -> true would be wrong, as a in the match condition is a rebound local variable, not the one we passed in the param list *)

(* 99 Problems in OCaml, https://ocaml.org/learn/tutorials/99problems.html *)

exception ListError;;

(* 1: return the last element of list *)

let rec return_last_element list = 
  match list with
    | a :: [] -> Some a
    | a :: b -> return_last_element b
    | [] -> None;;

let return_last_element_test1 = return_last_element [1;2;3;4];;
let return_last_element_test1 = return_last_element [1];;
let return_last_element_test1 = return_last_element [];;

(* Practice: return the last N elements of an array *)

(*
let return_last_n_elements list n = 
  match list with 
    | [] -> None
    | [a] -> None
    | h @ [a, b] -> [a, b];;

let return_last_two_element_test0 = return_last_n_elements [1;2;3];;
*)