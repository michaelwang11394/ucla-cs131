let rec inset item set = 
  match set with 
    | [] -> false
    | h :: t -> if item = h 
                  then true
                  else inset item t;;

let rec subset a b = 
  match a with
    | [] -> true
    | h :: t -> if inset h b
                  then subset t b
                  else false;;

let equal_sets a b =
  if subset a b && subset b a
    then true
    else false;;

let set_union a b = a @ b;;

let rec set_intersection a b =
  match a with
    | [] -> []
    | h :: t -> if inset h b
                  then h :: set_intersection t b
                  else set_intersection t b;;

let rec set_diff a b = 
  match a with
    | [] -> []
    | h :: t -> if inset h b
                  then set_diff t b
                  else h :: set_diff t b;;

(* Finding fixed point with 'best effort', as fixed point
   may not exist for given function and starting point 
   Per notes from Piazza: 'We will not test the case that causes stack overflow, which means the computed fixed point doesn't exist.' *)
let rec computed_fixed_point eq f x = 
  if eq (f x) x
    then x
    else computed_fixed_point eq f (f x);;

(* Calls function f(x) p times and return
   initial p as 0 is valid case, according to sample test cases. *)
let rec pcalls_x f p x = 
  if p = 0 
    then x
    else pcalls_x f (p-1) (f x);;

(* Same note as computed_fixed_point *)
let rec computed_periodic_point eq f p x = 
  if eq (pcalls_x f p x) x
    then x
    else computed_periodic_point eq f p (f x);;

(* filter_blind_alleys implementation *)

type ('n, 't) symbol = N of 'n | T of 't

(* helper functions *)

let is_symbol_terminal sym terminable_symbols = 
  match sym with
    | T s -> true
    | N s -> inset s terminable_symbols
    | _ -> false;;

let rec is_rule_rhs_terminable rhs terminable_symbols = 
  match rhs with
    | [] -> true
    | h :: t -> if is_symbol_terminal h terminable_symbols
                  then is_rule_rhs_terminable t terminable_symbols
                  else false;;

let rec filter_pass_rec rules terminable_symbols = 
  match rules with
    | [] -> terminable_symbols
    | (sym, rhs) :: t -> if is_rule_rhs_terminable rhs terminable_symbols
                           then filter_pass_rec t (sym::terminable_symbols)
                           else filter_pass_rec t (terminable_symbols);;

let filter_pass (rules, terminable_symbols) =
  rules, filter_pass_rec rules terminable_symbols;;

let equal_snd (start_one, a) (start_two, b) = 
  equal_sets a b;;

let rec final_pass rules terminable_symbols filtered_rules =
  match rules with
    | [] -> filtered_rules
    | (sym, rhs) :: t -> if is_rule_rhs_terminable rhs terminable_symbols
                           then final_pass t terminable_symbols (filtered_rules @ [(sym, rhs)])
                           else final_pass t terminable_symbols filtered_rules;;

let filter_blind_alleys (start_sym, rules) =
  start_sym, final_pass rules (snd(computed_fixed_point equal_snd filter_pass (rules, []))) [];;
  
