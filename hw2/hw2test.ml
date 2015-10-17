type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num

let awksub_rules_hw1 =
   [Expr, [T"("; N Expr; T")"];
    Expr, [N Num];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"]]

let test_convert_grammar_1 = ((snd (convert_grammar (Expr, awksub_rules_hw1))) Expr = [[T"("; N Expr; T")"]; [N Num]])
let test_convert_grammar_2 = ((snd (convert_grammar (Expr, awksub_rules_hw1))) Lvalue = [[T"$"; N Expr]])
let test_convert_grammar_3 = ((snd (convert_grammar (Expr, awksub_rules_hw1))) Num = [[T"0"]])


let accept_all derivation string = Some (derivation, string)
let accept_empty_suffix derivation = function
   | [] -> Some (derivation, [])
   | _ -> None

let awkish_grammar =
  (Expr,
   function
     | Expr ->
         [[N Term; N Binop; N Expr];
          [N Term]]
     | Term ->
	 [[N Num];
	  [N Lvalue];
	  [N Incrop; N Lvalue];
	  [N Lvalue; N Incrop];
	  [T"("; N Expr; T")"]]
     | Lvalue ->
	 [[T"$"; N Expr]]
     | Incrop ->
	 [[T"++"];
	  [T"--"]]
     | Binop ->
	 [[T"+"];
	  [T"-"]]
     | Num ->
	 [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
	  [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])

let test0 =
  ((parse_prefix awkish_grammar accept_all ["ouch"]) = None)

let test1 =
  ((parse_prefix awkish_grammar accept_all ["9"])
   = Some ([(Expr, [N Term]); (Term, [N Num]); (Num, [T "9"])], []))

let test2 =
  ((parse_prefix awkish_grammar accept_all ["9"; "+"; "$"; "1"; "+"])
   = Some
       ([(Expr, [N Term; N Binop; N Expr]); (Term, [N Num]); (Num, [T "9"]);
	 (Binop, [T "+"]); (Expr, [N Term]); (Term, [N Lvalue]);
	 (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Num]);
	 (Num, [T "1"])],
	["+"]))

let test3 =
  ((parse_prefix awkish_grammar accept_empty_suffix ["9"; "+"; "$"; "1"; "+"])
   = None)

(* This one might take a bit longer.... *)
let test4 =
 ((parse_prefix awkish_grammar accept_all
     ["("; "$"; "8"; ")"; "-"; "$"; "++"; "$"; "--"; "$"; "9"; "+";
      "("; "$"; "++"; "$"; "2"; "+"; "("; "8"; ")"; "-"; "9"; ")";
      "-"; "("; "$"; "$"; "$"; "$"; "$"; "++"; "$"; "$"; "5"; "++";
      "++"; "--"; ")"; "-"; "++"; "$"; "$"; "("; "$"; "8"; "++"; ")";
      "++"; "+"; "0"])
  = Some
     ([(Expr, [N Term; N Binop; N Expr]); (Term, [T "("; N Expr; T ")"]);
       (Expr, [N Term]); (Term, [N Lvalue]); (Lvalue, [T "$"; N Expr]);
       (Expr, [N Term]); (Term, [N Num]); (Num, [T "8"]); (Binop, [T "-"]);
       (Expr, [N Term; N Binop; N Expr]); (Term, [N Lvalue]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term; N Binop; N Expr]);
       (Term, [N Incrop; N Lvalue]); (Incrop, [T "++"]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term; N Binop; N Expr]);
       (Term, [N Incrop; N Lvalue]); (Incrop, [T "--"]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term; N Binop; N Expr]);
       (Term, [N Num]); (Num, [T "9"]); (Binop, [T "+"]); (Expr, [N Term]);
       (Term, [T "("; N Expr; T ")"]); (Expr, [N Term; N Binop; N Expr]);
       (Term, [N Lvalue]); (Lvalue, [T "$"; N Expr]);
       (Expr, [N Term; N Binop; N Expr]); (Term, [N Incrop; N Lvalue]);
       (Incrop, [T "++"]); (Lvalue, [T "$"; N Expr]); (Expr, [N Term]);
       (Term, [N Num]); (Num, [T "2"]); (Binop, [T "+"]); (Expr, [N Term]);
       (Term, [T "("; N Expr; T ")"]); (Expr, [N Term]); (Term, [N Num]);
       (Num, [T "8"]); (Binop, [T "-"]); (Expr, [N Term]); (Term, [N Num]);
       (Num, [T "9"]); (Binop, [T "-"]); (Expr, [N Term]);
       (Term, [T "("; N Expr; T ")"]); (Expr, [N Term]); (Term, [N Lvalue]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Lvalue]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Lvalue]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Lvalue; N Incrop]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Lvalue; N Incrop]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Incrop; N Lvalue]);
       (Incrop, [T "++"]); (Lvalue, [T "$"; N Expr]); (Expr, [N Term]);
       (Term, [N Lvalue; N Incrop]); (Lvalue, [T "$"; N Expr]); (Expr, [N Term]);
       (Term, [N Num]); (Num, [T "5"]); (Incrop, [T "++"]); (Incrop, [T "++"]);
       (Incrop, [T "--"]); (Binop, [T "-"]); (Expr, [N Term]);
       (Term, [N Incrop; N Lvalue]); (Incrop, [T "++"]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Lvalue; N Incrop]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]);
       (Term, [T "("; N Expr; T ")"]); (Expr, [N Term]);
       (Term, [N Lvalue; N Incrop]); (Lvalue, [T "$"; N Expr]); (Expr, [N Term]);
       (Term, [N Num]); (Num, [T "8"]); (Incrop, [T "++"]); (Incrop, [T "++"]);
       (Binop, [T "+"]); (Expr, [N Term]); (Term, [N Num]); (Num, [T "0"])],
      []))

let rec contains_lvalue = function
  | [] -> false
  | (Lvalue,_)::_ -> true
  | _::rules -> contains_lvalue rules

let accept_only_non_lvalues rules frag =
  if contains_lvalue rules
  then None
  else Some (rules, frag)

let test5 =
  ((parse_prefix awkish_grammar accept_only_non_lvalues
      ["3"; "-"; "4"; "+"; "$"; "5"; "-"; "6"])
   = Some
      ([(Expr, [N Term; N Binop; N Expr]); (Term, [N Num]); (Num, [T "3"]);
	(Binop, [T "-"]); (Expr, [N Term]); (Term, [N Num]); (Num, [T "4"])],
       ["+"; "$"; "5"; "-"; "6"]))

(* Added test cases *)

type language_of_bool_nonterminals = Expression | ElseClause

(* An ambiguous version of language_of_bool; testing parser behavior in an ambiguous grammar definition with blind alley rules *)
let ambiguous_language_of_bool = (Expression, function 
  | Expression -> [[T "if"; N Expression; T "then"; N Expression; T "else"; N Expression];
             [N ElseClause]; (* Blind alley but not left recursive *)
             [T "if"; N Expression; T "then"; N Expression]; (* ambiguous *)
             [T "True"];
             [T "False"]]
  | ElseClause -> [[T "else"; N ElseClause]])

let test_1 = ((parse_prefix ambiguous_language_of_bool accept_all 
  ["if";
     "if";"True";"then";
       "False";
     "else";
       "True";
   "then";
     "if";
       "False";
     "then";
       "False";
     "else";
       "if";"False";"then";
         "True";
       "else";
         "False"]) = Some ([
     (Expression, [T "if"; N Expression; T "then"; N Expression; T "else"; N Expression]);
     (Expression, [T "if"; N Expression; T "then"; N Expression; T "else"; N Expression]);
     (Expression, [T "True"]); (Expression, [T "False"]); (Expression, [T "True"]);
     (Expression, [T "if"; N Expression; T "then"; N Expression; T "else"; N Expression]);
     (Expression, [T "False"]); (Expression, [T "False"]);
     (Expression, [T "if"; N Expression; T "then"; N Expression]); (Expression, [T "False"]);
     (Expression, [T "True"]); (Expression, [T "False"])],
    []))

(* 
Because of ambiguity, the parser thinks of the above as 
  ["if";
     "if";"True";"then";
       "False";
     "else";
       "True";
   "then";
     "if";"False";"then";
       "False";
     "else";
       "if";"False";"then";"True";
   "else";
     "False"]
which is still the correct behavior; The parser still behaves Ok in an ambiguous grammar
*)

(* An unambiguous version of language_of_bool; testing S -> epsilon but without the blind alley rules above; tries accept_empty_suffix for fun *)
let unambiguous_language_of_bool = (Expression, function 
  | Expression -> [[T "if"; N Expression; T "then"; N Expression; N ElseClause];
             [T "True"];
             [T "False"]]
  | ElseClause -> [[T "else"; N Expression];
             []])

let test_2 = ((parse_prefix unambiguous_language_of_bool accept_empty_suffix 
  ["if";
     "if";"True";"then";
       "False";
     "else";
       "True";
   "then";
     "if";
       "False";
     "then";
       "False";
     "else";
       "if";"False";"then";
         "True";
       "else";
         "False"]) = Some ([
     (Expression,
      [T "if"; N Expression; T "then"; N Expression; N ElseClause]);
     (Expression,
      [T "if"; N Expression; T "then"; N Expression; N ElseClause]);
     (Expression, [T "True"]); (Expression, [T "False"]);
     (ElseClause, [T "else"; N Expression]); (Expression, [T "True"]);
     (Expression,
      [T "if"; N Expression; T "then"; N Expression; N ElseClause]);
     (Expression, [T "False"]); (Expression, [T "False"]);
     (ElseClause, [T "else"; N Expression]);
     (Expression,
      [T "if"; N Expression; T "then"; N Expression; N ElseClause]);
     (Expression, [T "False"]); (Expression, [T "True"]);
     (ElseClause, [T "else"; N Expression]); (Expression, [T "False"]);
     (ElseClause, [])],
    []))

(* Ambiguity is removed,  *)
