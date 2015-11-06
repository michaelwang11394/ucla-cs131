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

element_in_domain([], _).
element_in_domain([H|T], N) :- in_domain(H, N), element_in_domain(T, N).

all_in_domain([], _).
all_in_domain([H|T], N) :- element_in_domain(H, N), all_in_domain(T, N).

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

sum_matrix(_, [], 0).
/**
 * The sequence of "sum_matrix(X, T, Y1)" and "Y is Y1 + X2" does matter, putting the evaluation first would cause instantiation error
 */
sum_matrix(X, [I-J|T], Y) :- J1 is J - 1, I1 is I - 1, get_n(X, J1, X1), get_n(X1, I1, X2), sum_matrix(X, T, Y1), Y is Y1 + X2.

meet(X, +(Y, [H|T])) :- sum_matrix(X, [H|T], Y).

meet_constraints(X, []).
meet_constraints(X, [H|T]) :- meet(X, H), meet_constraints(X, T).

/**
 * Need to maplist first, then meet_constraints, otherwise it is trying to match some list values to meet constraints function
 */
kenken(N, C, T) :- length(T, N), length_match(T, N), all_in_domain(T, N), row_all_distinct(T), column_all_distinct(T, N), maplist(fd_labeling, T), meet_constraints(T, C).

sudoku(0, []).
sudoku(N, X) :- length(X, N), length_match(X, N), all_in_domain(X, N), row_all_distinct(X), column_all_distinct(X, N), maplist(fd_labeling, X).

/*
N1 is N - 1, extract_column([H|T], N1, Y), is_distinct(Y, [], true), sudoku(N1, T)
*/