type ('n, 't) symbol = N of 'n | T of 't

(*
  make_append_matchers tries to tackle "rule concatenation" as mentioned in class;
    Given a rule and fragments, check if this rule is applicable according to acceptor

  @param all_rules: 
    start_sym -> [rhs1; rhs2...],  where rhs# is [sym1; sym2...]
    All the rules from grammar. Used when we call 'or' as we see a non-terminal in the given rule, so that 'or' knows what the applicable rules are for this non-terminal symbol
  @param rule: 
    [sym1; sym2...]
    The rule (or what's left of it) that we want to apply for given symbols
  @param acceptor:
    derivation -> frag -> Some (derivation, frag) | None,  where derivation is [(lhs1, rhs12); (lhs2, rhs22)...] and frag is [term1; term2...]
    The acceptor function: Is the program's given acceptor when all previously matched symbols are terminal; Is "have to match later symbols" when we encounter a non-terminal and call 'or' for all its alternatives
    Reference: hint code, use one matcher as the acceptor of the other when concatenating;
  @param derivation:
    [(lhs1, rhs12); (lhs2, rhs22)...]
    The record(stack) of derivations that we've used if this matches; For recording what's applied and building acceptor curry for 'or'
  @param frag:
    [term1; term2...]
    The symbols that we want to test the given rule with; For recording what's left and building acceptor curry for 'or'

  @return:
    Some ([(lhs1, rhs12); (lhs2, rhs22)...], [term1; term2...]) | None
    Whatever the acceptor returns if the rule is accepted; None if the rule is not accepted
*)
let rec make_appended_matchers all_rules rule acceptor derivation frag = 
  match rule with 
    | [] -> acceptor derivation frag
    | rule_head :: rule_tail -> 
      (
        match rule_head with 
          | T(terminal) -> 
            (
              match frag with 
                | [] -> None
                | frag_head :: frag_tail -> 
                  (
                    if (frag_head = terminal) then 
                      (make_appended_matchers all_rules rule_tail acceptor derivation frag_tail)
                    else
                      None
                  )
            )
            (* Using "later terms has to match if 'or' wants to use certain alternative" as acceptor; this assumes the function signature of the acceptor *)
          | N(nonterminal) -> (make_or_matchers all_rules (all_rules nonterminal) nonterminal (make_appended_matchers all_rules rule_tail acceptor) frag derivation)
      )

(*
  make_or_matchers tries to tackle "rule alternatives" as mentioned in class;
    Given a set of rules and fragments, find the first rule that applies for the given acceptor

  @param all_rules: 
    start_sym -> [rhs1; rhs2...],  where rhs# is [sym1; sym2...]
    All the rules from grammar. Given to 'and' so that it knows what rules to give back to 'or' when it tries to match a non-terminal
  @param matching_rules: 
    start_sym -> [rhs1; rhs2...],  where rhs# is [sym1; sym2...]
    The rules (or what's left of them) that we can use for this call
  @param lhs:
    Type as defined in grammar
    The left hand side for this 'or'. Used for recording the past derivations
  @param acceptor:
    derivation -> frag -> Some (derivation, frag) | None,  where derivation is [(lhs1, rhs12); (lhs2, rhs22)...] and frag is [term1; term2...]
    The acceptor function: Is the program's given acceptor when all previously matched symbols are terminal; Is "have to match later symbols" when we encounter a non-terminal and call 'or' for all its alternatives
  @param derivation:
    [(lhs1, rhs12); (lhs2, rhs22)...]
    The record(stack) of derivations that we've used if this matches
  @param frag:
    [term1; term2...]
    The symbols that we want to test the given rule with

  @return:
    Some ([(lhs1, rhs12); (lhs2, rhs22)...], [term1; term2...]) | None
    Whatever the 'and' function returns; None if no rules are applicable.
*)
and make_or_matchers all_rules matching_rules lhs acceptor frag derivation = 
  match matching_rules with
    | [] -> None
    | rules_head :: rules_tail -> 
      (
        match make_appended_matchers all_rules rules_head acceptor (derivation @ [(lhs, rules_head)]) frag with
          | None -> 
            (make_or_matchers all_rules rules_tail lhs acceptor frag derivation)
          | any -> any 
      )

(*
 parse_prefix is the program entry; its semantic is similar with make_or_matchers
*)
let parse_prefix grammar acceptor frag = 
  match grammar with
    | (start_symbol, rules) -> make_or_matchers rules (rules start_symbol) start_symbol acceptor frag []