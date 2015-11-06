/**
 * Check if a certain element is in the given list
 * @param list
 * @param element
 * @param true | false
 */
has_element([], _, false).
has_element([H|_], H, true).
has_element([H|T], X, Y) :- H \= X, has_element(T, X, Y).

/**
 * Append a list to a given list
 * @param list1
 * @param list2
 * @param list1 + list2
 */
append_list([], X, X).
append_list([H|T], X, [H|TX]) :- append_list(T, X, TX).

/**
 * Check if numbers in a list are unique
 * @param test_list
 * @param helper_list, should use []
 * @param true | false
 * TODO: using my is_distinct results in stack overflow, when doing "maplist(domain_1_2, [H|T]), is_distinct([H|T], [], true), maplist(fd_labeling, [H|T])",
 *   Seems that this does not terminate correctly
 */
is_distinct([], _, true).
is_distinct([H|T], Y, false) :- has_element(Y, H, true).
is_distinct([H|T], Y, X) :- has_element(Y, H, false), append_list(Y, [H], YH), is_distinct(T, YH, X).

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
 * Note: This logic is kind of weird: 
 *  we get the i-th element E of the current row H, try to get the i-th 
 *  element of the next row with some list Y, and make sure Y + E gives 
 *  our given extracted array X; since we know E is an array with one or 
 *  zero element, we can append list Y to it
 * @param list of lists
 * @param column number
 * @param resulting list
 */
extract_column([], I, []).
extract_column([H|T], I, X) :- get_n(H, I, E), extract_column(T, I, Y), append_list([E], Y, X).


in_domain(X, N) :- fd_domain(X, 1, N).

list_in_domain([], _).
list_in_domain([H|T], N) :- in_domain(H, N), list_in_domain(T, N).

all_in_domain([], _).
all_in_domain([H|T], N) :- list_in_domain(H, N), all_in_domain(T, N).

row_all_distinct([]).
row_all_distinct([H|T]) :- fd_all_different(H), row_all_distinct(T).

/*
 * This function really only checks that all lists in a list has length N.
 * To enforce that length match gives n * n, instead of 1 * n, we check length(X, N) as well as length_match(X, N) in sudoku rhs
 *
 * Mistake: checked for length([H|T], N) at the beginning of recursive case, will give no as length [H|T] decreases as we recurse.
 */
length_match([H|T], N) :- length(H, N), length_match(T, N).
length_match([H], N) :- length(H, N).

/**
 * Differentiate this with column_all_distinct(_, 0). ?
 */
column_all_distinct(_, 0) :- !.
column_all_distinct(X, N) :- N1 is N - 1, extract_column(X, N1, T), fd_all_different(T), column_all_distinct(X, N1).

get_i_j(X, I, J, Y) :- J1 is J - 1, I1 is I - 1, get_n(X, I1, X1), get_n(X1, J1, Y).

/**
 * The sequence of "sum_matrix(X, T, Y1)" and "Y is Y1 + X2" does matter, putting the evaluation first would cause instantiation error
 */
sum_matrix(_, [], 0).
sum_matrix(X, [I-J|T], Y) :- get_i_j(X, I, J, E), sum_matrix(X, T, Y1), Y is Y1 + E.
multiply_matrix(_, [], 1).
multiply_matrix(X, [I-J|T], Y) :- get_i_j(X, I, J, E), multiply_matrix(X, T, Y1), Y is Y1 * E.

minus_matrix(X, I1-J1, I2-J2, Y) :- get_i_j(X, I1, J1, X1), get_i_j(X, I2, J2, X2), Y is X1 - X2.
divide_matrix(X, I1-J1, I2-J2, Y) :- get_i_j(X, I1, J1, X1), get_i_j(X, I2, J2, X2), Y is X1 / X2.

meet(X, +(Y, [H|T])) :- sum_matrix(X, [H|T], Y).
meet(X, *(Y, [H|T])) :- multiply_matrix(X, [H|T], Y).
meet(X, -(Y, P1, P2)) :- minus_matrix(X, P1, P2, Y); minus_matrix(X, P2, P1, Y).
meet(X, /(Y, P1, P2)) :- divide_matrix(X, P1, P2, Y); minus_matrix(X, P1, P2, Y).

meet_constraints(X, []).
meet_constraints(X, [H|T]) :- meet(X, H), meet_constraints(X, T).

/**
 * Need to maplist first, then meet_constraints, otherwise it is trying to match some list values to meet constraints function
 */
kenken(N, C, T) :- length(T, N), length_match(T, N), all_in_domain(T, N), row_all_distinct(T), column_all_distinct(T, N), maplist(fd_labeling, T), meet_constraints(T, C).

sudoku(0, []).
sudoku(N, X) :- length(X, N), length_match(X, N), all_in_domain(X, N), row_all_distinct(X), column_all_distinct(X, N), maplist(fd_labeling, X).

kenken_testcase(
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

kenken_testcase1(
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