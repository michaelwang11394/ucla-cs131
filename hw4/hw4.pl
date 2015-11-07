/*************************************************************************
 * KenKen (fd) - kenken/3
 *   @param N Size
 *   @param C Set of constraint
 *   @param T The result matrix
 *
 * This solution is rather fast: 
 *   Finding all solutions to Example kenken 6 took 7ms, while
 *   Example kenken 4 took 40ms
 *************************************************************************/
/**
 * Applying fd_domain to every element list of the given list (instead of
 * every element of each element list), and applying fd_labeling as the last 
 * seems to improve efficiency dramatically: commit d0ca39 could solve 4 * 4
 * , but could not solve 6 * 6 in 5 minutes; but this version could do it in ms
 *
 * Note: probably should use transpose instead of column_all_distinct, does
 * not seem to hurt efficiency much, so left here for now.
 */
kenken(N, C, T) :- 
  length(T, N), length_match(T, N), 
  maplist(in_domain(N), T), 
  meet_constraints(T, C), 
  row_all_distinct(T), column_all_distinct(T, N), 
  maplist(fd_labeling, T).

/*************************************************************************
 * Helper functions, could be replaced with built-in predicates
 *************************************************************************/
/**
 * Append a list to a given list
 * @param list1
 * @param list2
 * @param list1 + list2
 */
append_list([], X, X).
append_list([H|T], X, [H|TX]) :- append_list(T, X, TX).

/**
 * Get the i-th element from the list
 * @param list
 * @param index
 * @param element
 */
get_n([], I, []).
get_n([H|_], 0, H).
get_n([H|T], I, X) :- I \= 0, N is I - 1, get_n(T, N, X).

/**
 * Extract a column from a given matrix
 * @param list of lists
 * @param column number
 * @param resulting list
 */
extract_column([], I, []).
extract_column([H|T], I, X) :- 
  get_n(H, I, E), 
  extract_column(T, I, Y), 
  append_list([E], Y, X).

/**
 * This function really only checks that all lists in a list has length N.
 * To enforce that length match gives n * n, instead of 1 * n, we check length(X, N) as well as length_match(X, N) in sudoku rhs
 *
 * Mistake: checked for length([H|T], N) at the beginning of recursive case, will give no as length [H|T] decreases as we recurse.
 */
length_match([H|T], N) :- length(H, N), length_match(T, N).
length_match([H], N) :- length(H, N).

in_domain(N, L) :- fd_domain(L, 1, N).

/**
 * Get the element at i-th row and j-th column
 */
get_i_j(X, I, J, Y) :- 
  J1 is J - 1, I1 is I - 1, 
  get_n(X, I1, X1), get_n(X1, J1, Y).

/**
 * Differentiate this with column_all_distinct(_, 0). 
 */
column_all_distinct(_, 0) :- !.
column_all_distinct(X, N) :- 
  N1 is N - 1, 
  extract_column(X, N1, T), 
  fd_all_different(T), 
  column_all_distinct(X, N1).

row_all_distinct([]).
row_all_distinct([H|T]) :- 
  fd_all_different(H), 
  row_all_distinct(T).

/*************************************************************************
 * Numeric constraint functions
 *************************************************************************/
/**
 * The sequence of "sum_matrix(X, T, Y1)" and "Y is Y1 + X2" does matter,
 * putting the evaluation first would cause instantiation error; 
 * Differentiate '#=' and 'is': Using '#=' also solves the problem above
 */
sum_matrix(_, [], 0).
sum_matrix(X, [I-J|T], Y) :- 
  get_i_j(X, I, J, E), 
  sum_matrix(X, T, Y1), 
  Y #= Y1 + E.

multiply_matrix(_, [], 1).
multiply_matrix(X, [I-J|T], Y) :- 
  get_i_j(X, I, J, E), 
  multiply_matrix(X, T, Y1), 
  Y #= Y1 * E.

minus_matrix(X, I1-J1, I2-J2, Y) :- 
  get_i_j(X, I1, J1, X1), 
  get_i_j(X, I2, J2, X2), 
  Y #= X1 - X2.

divide_matrix(X, I1-J1, I2-J2, Y) :- 
  get_i_j(X, I1, J1, X1), 
  get_i_j(X, I2, J2, X2), 
  Y #= X1 / X2.

meet(X, +(Y, [H|T])) :- sum_matrix(X, [H|T], Y).
meet(X, *(Y, [H|T])) :- multiply_matrix(X, [H|T], Y).
meet(X, -(Y, P1, P2)) :- 
  minus_matrix(X, P1, P2, Y); 
  minus_matrix(X, P2, P1, Y).
meet(X, /(Y, P1, P2)) :- 
  divide_matrix(X, P1, P2, Y); 
  divide_matrix(X, P2, P1, Y).

meet_constraints(X, []).
meet_constraints(X, [H|T]) :- meet(X, H), meet_constraints(X, T).

/**
 * KenKen (fd) with no constraints, for fun and testing purposes
 */
sudoku(N, X) :- 
  length(X, N), length_match(X, N), 
  maplist(set_domain(N), X), 
  row_all_distinct(X), 
  column_all_distinct(X, N), 
  maplist(fd_labeling, X).

/*************************************************************************
 * KenKen (plain) - kenken/3
 * KenKen (plain) does not use fd_domain, fd_all_different, and fd_labeling
 *   The idea is to use permutations of int list instead of fd_domain
 *   Interesting note though, perm in class can check if something is a perm 
 *   of given list, but not list all the permutations, we use the built-in
 *   permutation for now
 *
 * This permutation-based solution is really slow:
 *   Finding all Solutions to Example 4 took 51s
 *************************************************************************/
plain_kenken(N, C, T) :- 
  length(T, N), length_match(T, N), get_domain_list(N, R),
  plain_domain(T, N),
  meet_constraints(T, C), plain_column_all_distinct(T, N, R).

/**
 * Note: the difference between putting append_list(R, [N], R1) and append_list
 * (R1, [N], R) here...why do we have to add length(R, N)?
 */
get_domain_list(0, _) :- !.
get_domain_list(N, R) :-
  length(R, N),
  N1 is N - 1,
  append_list(R1, [N], R),
  get_domain_list(N1, R1).

plain_domain([], _) :- !.
plain_domain([H|T], N) :-
  get_domain_list(N, P), 
  permutation(P, H),
  plain_domain(T, N).

plain_column_all_distinct(_, 0, _) :- !.
plain_column_all_distinct(X, N, R) :- 
  N1 is N - 1, 
  extract_column(X, N1, T), 
  permutation(T, R),
  plain_column_all_distinct(X, N1, R).

/*************************************************************************
 * KenKen built-in test cases
 *************************************************************************/
zhehao_kenken_testcase(
  4,
  [
   +(6, [1-1, 1-2, 2-1]),
   *(96, [1-3, 1-4, 2-2, 2-3, 2-4]),
   -(1, 3-1, 3-2),
   -(1, 4-1, 4-2),
   +(8, [3-3, 4-3, 4-4]),
   *(2, [3-4])
  ]
).

zhehao_kenken_testcase1(
  6,
  [
   +(11, [1-1, 2-1]),
   /(2, 1-2, 1-3),
   *(20, [1-4, 2-4]),
   *(6, [1-5, 1-6, 2-6, 3-6]),
   -(3, 2-2, 2-3),
   /(3, 2-5, 3-5),
   *(240, [3-1, 3-2, 4-1, 4-2]),
   *(6, [3-3, 3-4]),
   *(6, [4-3, 5-3]),
   +(7, [4-4, 5-4, 5-5]),
   *(30, [4-5, 4-6]),
   *(6, [5-1, 5-2]),
   +(9, [5-6, 6-6]),
   +(8, [6-1, 6-2, 6-3]),
   /(2, 6-4, 6-5)
  ]
).