type ('n, 't) symbol = N of 'n | T of 't

(*
let get_rhs grammar start_symbol = 
  let produce nonterminal = 
    (snd grammar) nonterminal
  in produce start_symbol;;

let rec traverse grammar start_symbol depth = 
  List.iter (fun lst -> (
      Printf.printf "lst\n"; 
      List.iter (fun x -> (traverse x depth)) lst;
    )
  ) (get_rhs grammar start_symbol);;  
*)

let rec make_appended_matchers all_rules rule acceptor derivation frag = 
  match rule with 
    | [] -> acceptor derivation frag
    | rule_head :: rule_tail -> 
      (match rule_head with 
        | T(terminal) -> 
          (match frag with 
            | [] -> 
              (None)
            | frag_head :: frag_tail -> 
              (if (frag_head = terminal) then 
                (make_appended_matchers all_rules rule_tail acceptor derivation frag_tail)
              else
                None)
          )
        | N(nonterminal) -> (make_or_matchers all_rules (all_rules nonterminal) nonterminal (make_appended_matchers all_rules rule_tail acceptor) frag derivation)
      )

and make_or_matchers all_rules matching_rules lhs acceptor frag derivation = 
  match matching_rules with
    | [] -> None
    | rules_head :: rules_tail -> 
      (match make_appended_matchers all_rules rules_head acceptor (derivation @ [(lhs, rules_head)]) frag with
        | None -> 
          (make_or_matchers all_rules rules_tail lhs acceptor frag derivation)
        | any -> any 
      )

let parse_prefix grammar acceptor frag = 
  match grammar with
    | (start_symbol, rules) -> make_or_matchers rules (rules start_symbol) start_symbol acceptor frag []