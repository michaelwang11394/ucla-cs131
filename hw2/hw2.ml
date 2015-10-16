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


let rec make_appended_matchers other_rules rule acceptor frag result = 
  match rule with 
    | [] -> acceptor result frag
    | rulehead :: ruletail -> 
      (match rulehead with 
        | T(terminal) -> 
          (match frag with 
            | [] -> None
            | fraghead :: fragtail -> 
              (if (fraghead = terminal) then 
                (make_appended_matchers other_rules ruletail acceptor fragtail result)
              else
                None)
          )
        | N(nonterminal) -> (make_or_matchers other_rules acceptor frag result)
      )

and make_or_matchers matching_rules acceptor frag result = 
  match matching_rules with
    | [] -> None
    | head :: tail -> 
      (match make_appended_matchers tail head acceptor frag result with
        | None -> make_or_matchers tail acceptor frag result
        | any -> any 
      )

let parse_prefix grammar acceptor frag = 
  match grammar with
    | (start_symbol, rules) -> make_or_matchers (rules start_symbol) acceptor frag []