#!/usr/bin/env -S swipl
%-*- mode: prolog; -*-

:- module(prj4_sol, [
      sublist_lengths/2,
      same_length_sublists/1,
      fibonacci_sublists/1,
      assoc_lookup/3,
      assoc_replace/3,
      add_to_plus_expr/2,
      named_to_op_expr/2,
      named_expr_eval/2,
      named_expr_to_prefix_tokens/2,
      op_expr_to_prefix_tokens/2
  ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%% sublist_lengths/2 %%%%%%%%%%%%%%%%%%%%%%%%%%%

% #1: 10-points

% sublist_lengths(List, Lengths) should succeed iff List is a list
% containing sub-lists and Lengths is a list having the same length
% as List and whose elements are the lengths of the corresponding
% sub-list in List.  You may assume that all the elements of List
% are sub-lists.

% Base case: empty list corresponds to an empty lengths list.
sublist_lengths([], []).

% Recursive case: Compute the length of the head sublist and recurse on the tail.
sublist_lengths([Head|Tail], [Len|LengthsTail]) :-
    length(Head, Len),
    sublist_lengths(Tail, LengthsTail).

:-begin_tests(sublist_lengths, []).
test(empty, [nondet]) :-
    sublist_lengths([], Lengths), Lengths = [].
test(sublist_lengths1, [nondet]) :-
    sublist_lengths([[a, b]], Lengths), Lengths = [2].
test(sublist_lengths3, [nondet]) :-
    sublist_lengths([[2], [a, b], [x, y, z]], Lengths), Lengths = [1, 2, 3].
test(sublist_lengths_var_list, [nondet]) :-
    sublist_lengths(List, [1, 2, 3]), length(List, 3).
:-end_tests(sublist_lengths).

%%%%%%%%%%%%%%%%%%%%%%%%% same_length_sublists/1 %%%%%%%%%%%%%%%%%%%%%%%%

% #2: 10-points

% same_length_sublists(List): succeed only iff all the sublists in
% List have the same length.  You may assume that all the elements of
% List are sub-lists.  The procedure should succeed for an empty List.
%
% *Hint*: use an auxiliary procedure.

% Base case: An empty list always has sublists of the same length (trivially true).
same_length_sublists([]).

% Use auxiliary predicate to verify all sublists have the same length as the first sublist.
same_length_sublists([First|Rest]) :-
    length(First, Len),
    all_sublists_same_length(Rest, Len).

% Base case for the auxiliary predicate: No more sublists to check.
all_sublists_same_length([], _).

% Recursive case: Check if the current sublist has the length Len, and recurse for the rest.
all_sublists_same_length([Head|Tail], Len) :-
    length(Head, Len),
    all_sublists_same_length(Tail, Len).

:-begin_tests(same_length_sublists, []).
test(empty, [nondet]) :-
    same_length_sublists([]).
test(empties, [nondet]) :-
    same_length_sublists([[], [], []]).
test(empties_fail, [fail]) :-
    same_length_sublists([[], [2], []]).
test(sublists1, [nondet]) :-
    same_length_sublists([[[a, 2]], [[]], [c]]).
test(sublists1_fail, [fail]) :-
    same_length_sublists([[a], [[]], [c, 2]]).
test(sublists3, [nondet]) :-
    same_length_sublists([[a, [2], 4], [b, 5, [1]], [3, 2, c]]).
test(sublists3_fail, [fail]) :-
    same_length_sublists([[a, 2, 4], [b, 5, 1], [3, [2, c]]]).
:-end_tests(same_length_sublists).


%%%%%%%%%%%%%%%%%%%%%%%%%% fibonacci_sublists/1 %%%%%%%%%%%%%%%%%%%%%%%%%

% #3: 10-points

% fibonacci_sublists(List) should succeed iff List is a list of
% sublists whose lengths have a Fibonacci relationship; i.e.
% length(List[i]) == length(List[i-2]) + length(List[i-1])
% where List[i] is the sublist at index i in List.  You may
% assume that List contains only sublists.  The procedure
% should trivially succeed if the length of List is < 3.

% Base case: Succeed if the list has fewer than three sublists.
fibonacci_sublists(List) :-
    length(List, Len),
    Len < 3.

% Recursive case: Check the Fibonacci condition from the third sublist onwards.
fibonacci_sublists([First, Second|Rest]) :-
    length(First, Len1),
    length(Second, Len2),
    check_fibonacci(Rest, Len1, Len2).

% Base case for auxiliary predicate: No more sublists to check.
check_fibonacci([], _, _).

% Recursive case: Check Fibonacci condition and continue with the next sublist.
check_fibonacci([Head|Tail], Len1, Len2) :-
    length(Head, Len3),
    Len3 =:= Len1 + Len2,  % Use arithmetic equality
    check_fibonacci(Tail, Len2, Len3).

:-begin_tests(fibonacci_sublists, []).
test(empty, [nondet]) :-
    fibonacci_sublists([]).
test(zero, [nondet]) :-
    fibonacci_sublists([[]]).
test(one, [nondet]) :-
    fibonacci_sublists([[], [a]]).
test(two, [nondet]) :-
    fibonacci_sublists([[], [a], [c]]).
test(three, [nondet]) :-
    fibonacci_sublists([[], [a], [c], [a, c]]).
test(three_fail, [fail]) :-
    fibonacci_sublists([[], [a], [c], [a, c, c]]).
test(four, [nondet]) :-
    fibonacci_sublists([[], [a], [c], [a, c], [1, 2, 3] ]).
test(four_fail, [fail]) :-
    fibonacci_sublists([[], [a], [c], [a, c], [1, 2, 3, 4] ]).
test(ten, [nondet]) :-
    fibonacci_sublists([[], [a], [c], [a, c], [1, 2, 3], [1, 2, 3, 4, 5],
			Eight, Thirteen, TwentyOne, ThirtyFour, FiftyFive]),
    length(Eight, 8),
    length(Thirteen, 13),
    length(TwentyOne, 21),
    length(ThirtyFour, 34),
    length(FiftyFive, 55).
test(ten_fail, [fail]) :-
    fibonacci_sublists([[], [a], [c], [a, c], [1, 2, 3], [1, 2, 3, 4, 5],
			Eight, Thirteen, TwentyOne, ThirtyFour, FiftySix]),
    !, %prevent backtracking
    length(Eight, 8),
    length(Thirteen, 13),
    length(TwentyOne, 21),
    length(ThirtyFour, 34),
    length(FiftySix, 56).
test(four_start_22, [nondet]) :-
    fibonacci_sublists([[1, 2], [1, 2], [1, 2, 3, 4], [1, 2, 3, 4, 5, 6]]).
test(four_start_22_fail, [fail]) :-
    fibonacci_sublists([[1, 2], [1, 2], [1, 2, 3, 4], [1, 2, 3, 4, 5]]).
:-end_tests(fibonacci_sublists).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%% assoc_lookup/3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%

% #4: 5-points

% A "association list" is a list of (Key, Value) pairs and can
% be used as a dictionary.

% assoc_lookup(Assoc, Key, Value): succeeds if Value is associated with
% Key in association list Assoc.
% *Restriction*: you may not use recursion.
% *Hint* your solution should simply call a Prolog built-in.

% Using member/2 to find the (Key, Value) pair in the association list.
assoc_lookup(Assoc, Key, Value) :-
    member((Key, Value), Assoc).

:-begin_tests(assoc_lookup, []).
test(empty, [fail]) :-
    assoc_lookup([], key, _Value).
test(first, [nondet]) :-
    assoc_lookup([(key, 42), (a, 22), (b, 33)], key, Value),
    42 = Value.
test(last, [nondet]) :-
    assoc_lookup([(a, 22), (b, 33), (key, 42)], key, Value),
    Value = 42.
test(mid, [nondet]) :-
    assoc_lookup([(a, 22), (key, 42), (b, 33)], key, Value),
    42 = Value.
test(multi, [nondet]) :-
    assoc_lookup([(a, 22), (key, 42), (b, 33), (key, 22) ], key, Value),
    Value = 42.
test(multi_fail, [fail]) :-
    assoc_lookup([(a, 22), (key, 42), (b, 33), (key, 22) ], key, Value),
    43 = Value.
test(bound_value, [nondet]) :-
    assoc_lookup([(a, 22), (key, 42), (b, 33), (key, 22) ], key, 22).
test(unbound_key, [nondet]) :-
    assoc_lookup([(a, 22), (key, 42), (b, 33), (key, 22) ], Key, 33),
    b = Key.
:-end_tests(assoc_lookup).

%%%%%%%%%%%%%%%%%%%%%%%%%%%% assoc_replace/3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%

% #5: 10-points

% assoc_replace(AtomIntList, Assoc, ListZ): given a list AtomIntList
% containing Prolog atoms and integers, match ListZ with the list
% which is the result of replacing all atoms in AtomIntList with their
% value in assoc-list Assoc.
%
% *Hints*: Use assoc_lookup/3 from your answer to the previous
% exercise and Prolog's built-ins atom(A) which succeeds if A is an
% atom and integer(I) which succeeds if I is an integer.

% Base case: Empty list returns an empty result list.
assoc_replace([], _, []).

% Case for atoms: Replace atom with associated value if found.
assoc_replace([Head|Tail], Assoc, [ZHead|ZTail]) :-
    atom(Head),
    assoc_lookup(Assoc, Head, ZHead), % Use previously defined predicate to look up value
    assoc_replace(Tail, Assoc, ZTail).

% Case for integers: Retain the integer in the result list.
assoc_replace([Head|Tail], Assoc, [Head|ZTail]) :-
    integer(Head),
    assoc_replace(Tail, Assoc, ZTail).

% Case when an atom has no association and must cause the predicate to fail.
assoc_replace([Head|_], Assoc, _) :-
    atom(Head),
    \+ assoc_lookup(Assoc, Head, _), % Check that the atom has no associated value
    fail.

:-begin_tests(assoc_replace, []).
test(empty, [nondet]) :-
    assoc_replace([], [(a,22), (b, 33), (c, 42)], Z),
    Z = [].
test(single, [nondet]) :-
    assoc_replace([c], [(a,22), (b, 33), (c, 42)], Z),
    Z = [42].
test(none, [nondet]) :-
    assoc_replace([77], [(a,22), (b, 33), (c, 42)], Z),
    Z = [77].
test(multi, [nondet]) :-
    assoc_replace([c, a, 8, b, 44], [(a,22), (b, 33), (c, 42)], Z),
    Z = [42, 22, 8, 33, 44].
test(multi_fail, [fail]) :-
    assoc_replace([c, a, d, b, 44], [(a,22), (b, 33), (c, 42)], Z),
    Z = [42, 22, d, 33, 44].
:-end_tests(assoc_replace).

%%%%%%%%%%%%%%%%%%%%%%%%%%% add_to_plus_expr/2 %%%%%%%%%%%%%%%%%%%%%%%%%%

% #6: 10-points

% An add-expr is an integer or of the form add(X, Y), where X and
% Y are add-expr's.  A plus-expr is an integer or of the form +(X, Y),
% where X and Y are plus-expr's (note that +(X, Y) can also be
% written in Prolog as X + Y).
%
% add_to_plus_expr(AddExpr, PlusExpr) should succeed iff PlusExpr
% is the same as AddExpr with each add replaced by +.
%
% *Hint*: the Prolog built-in integer(I) succeeds iff I is an integer.

% Handle integers directly as they do not require any transformation.
add_to_plus_expr(Expr, Expr) :- 
    integer(Expr).

% Recursive transformation from custom add expression to native plus expression.
add_to_plus_expr(add(X, Y), PlusExpr) :-
    nonvar(X), nonvar(Y),        % Ensure subexpressions are instantiated
    add_to_plus_expr(X, XPlus),  % Recursively transform the left part of the add expression
    add_to_plus_expr(Y, YPlus),  % Recursively transform the right part of the add expression
    PlusExpr = XPlus + YPlus.    % Combine transformed parts using Prolog's native plus

% Recursive transformation from native plus expression to custom add expression.
% This direction only activates if the Expr is explicitly a structure of the form X + Y.
add_to_plus_expr(AddExpr, X + Y) :-
    nonvar(X), nonvar(Y),        % Ensure subexpressions are instantiated
    add_to_plus_expr(XAdd, X),   % Recursively transform the left part of the plus expression
    add_to_plus_expr(YAdd, Y),   % Recursively transform the right part of the plus expression
    AddExpr = add(XAdd, YAdd).   % Assemble the transformed parts into a custom add expression

:-begin_tests(add_to_plus_expr, []).
test(int, [nondet]) :-
    add_to_plus_expr(42, Z), Z = 42.
test(add_2_3, [nondet]) :-
    add_to_plus_expr(add(2, 3), Z), Z = 2 + 3.
test(add_add_2_3_add_4_5, [nondet]) :-
    add_to_plus_expr(add(add(2, 3), add(4, 5)), Z), Z = (2 + 3) + (4 + 5).
test(add_add_add_add_1_2_3_4_5, [nondet]) :-
    add_to_plus_expr(add(add(add(add(1, 2), 3), 4), 5), Z),
    Z = 1 + 2 + 3 + 4 + 5.
test(add_add_add_add_1_2_3_4_5_fail, [fail]) :-
    add_to_plus_expr(add(add(add(add(1, 2), 3), 4), 5), Z),
    Z = 1 + 2 + 3 + (4 + 5).
test(add_1_add_2_add_3_add_4_5, [nondet]) :-
    add_to_plus_expr(add(1, add(2, add(3, add(4, 5)))), Z),
    Z = 1 + (2 + (3 + (4 + 5))).

% reversed instantiation patterns
test(rev_int, [nondet]) :-
    add_to_plus_expr(Z, 42), Z = 42.
test(rev_add_2_3, [nondet]) :-
    add_to_plus_expr(Z, 2 + 3), Z = add(2, 3).
test(rev_add_add_2_3_add_4_5, [nondet]) :-
    add_to_plus_expr(Z, (2 + 3) + (4 + 5)), Z = add(add(2, 3), add(4, 5)).
test(rev_add_add_add_add_1_2_3_4_5, [nondet]) :-
    add_to_plus_expr(Z,  1 + 2 + 3 + 4 + 5),
    Z = add(add(add(add(1, 2), 3), 4), 5).
test(rev_add_add_add_add_1_2_3_4_5_fail, [fail]) :-
    add_to_plus_expr(Z, 1 + 2 + 3 + (4 + 5)),
    Z = add(add(add(add(1, 2), 3), 4), 5).
test(rev_add_1_add_2_add_3_add_4_5, [nondet]) :-
    add_to_plus_expr(Z, 1 + (2 + (3 + (4 + 5)))),
    Z = add(1, add(2, add(3, add(4, 5)))).    
:-end_tests(add_to_plus_expr).

%%%%%%%%%%%%%%%%%%%%%%%%%%% named_to_op_expr/2 %%%%%%%%%%%%%%%%%%%%%%%%%%

% #7: 10-points

% A named-expr is either a integer, or is one of add(X, Y) or
% mul(X, Y) where X and Y are named-expr's.  An op-expr
% is an arithmetic expression over integers and binary operators + and
% *.
%
% named_to_op_expr(NamedExpr, OpExpr) should succeed iff OpExpr
% is the same as NamedExpr with each add and mul replaced by
% + and * respectively.
% It should be possible to run this procedure with either one or
% both arguments instantiated.

% Handle integers directly as they do not require any transformation.
named_to_op_expr(Expr, Expr) :- 
    integer(Expr).

% Transform 'add' named-expr to '+' op-expr.
named_to_op_expr(add(X, Y), OpExpr) :-
    nonvar(X), nonvar(Y),        % Ensure subexpressions are instantiated
    named_to_op_expr(X, XOp),    % Recursively transform the left part
    named_to_op_expr(Y, YOp),    % Recursively transform the right part
    OpExpr = XOp + YOp.          % Combine using Prolog's native '+'

% Transform 'mul' named-expr to '*' op-expr.
named_to_op_expr(mul(X, Y), OpExpr) :-
    nonvar(X), nonvar(Y),        % Ensure subexpressions are instantiated
    named_to_op_expr(X, XOp),    % Recursively transform the left part
    named_to_op_expr(Y, YOp),    % Recursively transform the right part
    OpExpr = XOp * YOp.          % Combine using Prolog's native '*'

% Reverse transformation from '+' op-expr to 'add' named-expr.
named_to_op_expr(NamedExpr, X + Y) :-
    nonvar(X), nonvar(Y),        % Ensure subexpressions are instantiated
    named_to_op_expr(XNamed, X), % Recursively transform the left part
    named_to_op_expr(YNamed, Y), % Recursively transform the right part
    NamedExpr = add(XNamed, YNamed).  % Assemble into 'add'

% Reverse transformation from '*' op-expr to 'mul' named-expr.
named_to_op_expr(NamedExpr, X * Y) :-
    nonvar(X), nonvar(Y),        % Ensure subexpressions are instantiated
    named_to_op_expr(XNamed, X), % Recursively transform the left part
    named_to_op_expr(YNamed, Y), % Recursively transform the right part
    NamedExpr = mul(XNamed, YNamed).  % Assemble into 'mul'

:-begin_tests(named_to_op_expr, []).
test(int, [nondet]) :-
    NamedExpr = 42, OpExpr = 42,
    named_to_op_expr(NamedExpr, Z),
    Z = OpExpr.

test(add_2_3, [nondet]) :-
    NamedExpr = add(2, 3), OpExpr = 2 + 3,
    named_to_op_expr(NamedExpr, Z),
    Z = OpExpr.
test(add_add_2_3_add_4_5, [nondet]) :-
    NamedExpr = add(add(2, 3), add(4, 5)), OpExpr = (2 + 3) + (4 + 5),
    named_to_op_expr(NamedExpr, Z),
    Z = OpExpr.
test(add_add_add_add_1_2_3_4_5, [nondet]) :-
    NamedExpr = add(add(add(add(1, 2), 3), 4), 5), OpExpr = 1 + 2 + 3 + 4 + 5,
    named_to_op_expr(NamedExpr, Z),
    Z = OpExpr.
test(add_add_add_add_1_2_3_4_5_fail, [fail]) :-
    NamedExpr = add(add(add(add(1, 2), 3), 4), 5), OpExpr = 1 + 2 + 3 + (4 + 5),
    named_to_op_expr(NamedExpr, Z),
    Z = OpExpr.
test(add_1_add_2_add_3_add_4_5, [nondet]) :-
    NamedExpr = add(1, add(2, add(3, add(4, 5)))),
    OpExpr = 1 + (2 + (3 + (4 + 5))), 
    named_to_op_expr(NamedExpr, Z),
    Z = OpExpr.
		     

test(mul_2_3, [nondet]) :-
    NamedExpr = mul(2, 3), OpExpr = 2 * 3,
    named_to_op_expr(NamedExpr, Z),
    Z = OpExpr.
test(mul_mul_2_3_mul_4_5, [nondet]) :-
    NamedExpr = mul(mul(2, 3), mul(4, 5)), OpExpr = (2 * 3) * (4 * 5),
    named_to_op_expr(NamedExpr, Z),
    Z = OpExpr.
test(mul_mul_mul_mul_1_2_3_4_5, [nondet]) :-
    NamedExpr = mul(mul(mul(mul(1, 2), 3), 4), 5), OpExpr = 1 * 2 * 3 * 4 * 5,
    named_to_op_expr(NamedExpr, Z),
    Z = OpExpr.
test(mul_mul_mul_mul_1_2_3_4_5_fail, [fail]) :-
    NamedExpr = mul(mul(mul(mul(1, 2), 3), 4), 5),
    OpExpr = 1 * 2 * 3 * (4 * 5),
    named_to_op_expr(NamedExpr, Z),
    Z = OpExpr.
test(mul_1_mul_2_mul_3_mul_4_5, [nondet]) :-
    NamedExpr = mul(1, mul(2, mul(3, mul(4, 5)))),
    OpExpr = 1 * (2 * (3 * (4 * 5))),
    named_to_op_expr(NamedExpr, Z),
    Z = OpExpr.

test(mul_add_1_mul_2_3, [nondet]) :-
    NamedExpr = mul(add(1, 2), 3), OpExpr = (1 + 2) * 3,
    named_to_op_expr(NamedExpr, Z),
    Z = OpExpr.
test(add_1_mul_2_3, [nondet]) :-
    NamedExpr = add(1, mul(2, 3)), OpExpr = 1 + 2*3,
    named_to_op_expr(NamedExpr, Z),
    Z = OpExpr.
test(add_mul_1_2_add_3_4, [nondet]) :-
    NamedExpr = add(mul(1, 2), mul(3, 4)), OpExpr = 1*2 + 3*4,
    named_to_op_expr(NamedExpr, Z),
    Z = OpExpr.
test(mul_add_1_2_mul_3_4, [nondet]) :-
    NamedExpr = mul(add(1, 2), add(3, 4)), OpExpr = (1 + 2) * (3 + 4),
    named_to_op_expr(NamedExpr, Z),
    Z = OpExpr.

% reversed instantiation patterns
test(rev_int, [nondet]) :-
    NamedExpr = 42, OpExpr = 42,
    named_to_op_expr(Z, OpExpr),
    Z = NamedExpr.

test(rev_add_2_3, [nondet]) :-
    NamedExpr = add(2, 3), OpExpr = 2 + 3,
    named_to_op_expr(Z, OpExpr),
    Z = NamedExpr.
test(rev_add_add_2_3_add_4_5, [nondet]) :-
    NamedExpr = add(add(2, 3), add(4, 5)), OpExpr = (2 + 3) + (4 + 5),
    named_to_op_expr(Z, OpExpr),
    Z = NamedExpr.
test(rev_add_add_add_add_1_2_3_4_5, [nondet]) :-
    NamedExpr = add(add(add(add(1, 2), 3), 4), 5), OpExpr = 1 + 2 + 3 + 4 + 5,
    named_to_op_expr(Z, OpExpr),
    Z = NamedExpr.
test(rev_add_add_add_add_1_2_3_4_5_fail, [fail]) :-
    NamedExpr = add(add(add(add(1, 2), 3), 4), 5), OpExpr = 1 + 2 + 3 + (4 + 5),
    named_to_op_expr(Z, OpExpr),
    Z = NamedExpr.
test(rev_add_1_add_2_add_3_add_4_5, [nondet]) :-
    NamedExpr = add(1, add(2, add(3, add(4, 5)))),
    OpExpr = 1 + (2 + (3 + (4 + 5))), 
    named_to_op_expr(Z, OpExpr),
    Z = NamedExpr.
		     

test(rev_mul_2_3, [nondet]) :-
    NamedExpr = mul(2, 3), OpExpr = 2 * 3,
    named_to_op_expr(Z, OpExpr),
    Z = NamedExpr.
test(rev_mul_mul_2_3_mul_4_5, [nondet]) :-
    NamedExpr = mul(mul(2, 3), mul(4, 5)), OpExpr = (2 * 3) * (4 * 5),
    named_to_op_expr(Z, OpExpr),
    Z = NamedExpr.
test(rev_mul_mul_mul_mul_1_2_3_4_5, [nondet]) :-
    NamedExpr = mul(mul(mul(mul(1, 2), 3), 4), 5), OpExpr = 1 * 2 * 3 * 4 * 5,
    named_to_op_expr(Z, OpExpr),
    Z = NamedExpr.
test(rev_mul_mul_mul_mul_1_2_3_4_5_fail, [fail]) :-
    NamedExpr = mul(mul(mul(mul(1, 2), 3), 4), 5),
    OpExpr = 1 * 2 * 3 * (4 * 5),
    named_to_op_expr(Z, OpExpr),
    Z = NamedExpr.
test(rev_mul_1_mul_2_mul_3_mul_4_5, [nondet]) :-
    NamedExpr = mul(1, mul(2, mul(3, mul(4, 5)))),
    OpExpr = 1 * (2 * (3 * (4 * 5))),
    named_to_op_expr(Z, OpExpr),
    Z = NamedExpr.

test(rev_mul_add_1_mul_2_3, [nondet]) :-
    NamedExpr = mul(add(1, 2), 3), OpExpr = (1 + 2) * 3,
    named_to_op_expr(Z, OpExpr),
    Z = NamedExpr.
test(rev_add_1_mul_2_3, [nondet]) :-
    NamedExpr = add(1, mul(2, 3)), OpExpr = 1 + 2*3,
    named_to_op_expr(Z, OpExpr),
    Z = NamedExpr.
test(rev_add_mul_1_2_add_3_4, [nondet]) :-
    NamedExpr = add(mul(1, 2), mul(3, 4)), OpExpr = 1*2 + 3*4,
    named_to_op_expr(Z, OpExpr),
    Z = NamedExpr.
test(rev_mul_add_1_2_mul_3_4, [nondet]) :-
    NamedExpr = mul(add(1, 2), add(3, 4)), OpExpr = (1 + 2) * (3 + 4),
    named_to_op_expr(Z, OpExpr),
    Z = NamedExpr.

:-end_tests(named_to_op_expr).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% named_expr_eval/2 %%%%%%%%%%%%%%%%%%%%%%%%%

% #8: 10-points

% named_expr_eval(NamedExpr, Value): Value matches the result of evaluating
% named-expr NamedExpr (named-expr is as in the previous question, add
% should add its operands and mul should multiply them).
%
% *Hint*: combine your solution to the previous exercise with is/2.

% Base case: if NamedExpr is a plain integer, it is directly the Value.
named_expr_eval(Expr, Expr) :- 
    integer(Expr).

% Recursive evaluation for 'add' expressions.
named_expr_eval(add(X, Y), Value) :-
    named_expr_eval(X, EvalX),  % Recursively evaluate the left operand
    named_expr_eval(Y, EvalY),  % Recursively evaluate the right operand
    Value is EvalX + EvalY.     % Perform addition of the evaluated operands

% Recursive evaluation for 'mul' expressions.
named_expr_eval(mul(X, Y), Value) :-
    named_expr_eval(X, EvalX),  % Recursively evaluate the left operand
    named_expr_eval(Y, EvalY),  % Recursively evaluate the right operand
    Value is EvalX * EvalY.     % Perform multiplication of the evaluated operands

:-begin_tests(named_expr_eval, []).
test(int, [nondet]) :-
    named_expr_eval(42, 42).

test(add_2_3, [nondet]) :-
    named_expr_eval(add(2, 3), 5).
test(add_add_2_3_add_4_5, [nondet]) :-
    named_expr_eval(add(add(2, 3), add(4, 5)), 14).
test(add_add_add_add_1_2_3_4_5, [nondet]) :-
    named_expr_eval(add(add(add(add(1, 2), 3), 4), 5), 15).
test(add_add_add_add_1_2_3_4_5_fail, [fail]) :-
    named_expr_eval(add(add(add(add(1, 2), 3), 4), 5), 16).
test(add_1_add_2_add_3_add_4_5, [nondet]) :-
    named_expr_eval(add(1, add(2, add(3, add(4, 5)))), 15).

test(mul_2_3, [nondet]) :-
    named_expr_eval(mul(2, 3), 6).
test(mul_mul_2_3_mul_4_5, [nondet]) :-
    named_expr_eval(mul(mul(2, 3), mul(4, 5)), 120).
test(mul_mul_mul_mul_1_2_3_4_5, [nondet]) :-
    named_expr_eval(mul(mul(mul(mul(1, 2), 3), 4), 5), 120).
test(mul_mul_mul_mul_1_2_3_4_5_fail, [fail]) :-
    named_expr_eval(mul(mul(mul(mul(1, 2), 3), 4), 5), 121).
test(mul_1_mul_2_mul_3_mul_4_5, [nondet]) :-
    named_expr_eval(mul(1, mul(2, mul(3, mul(4, 5)))), 120).

test(mul_add_1_mul_2_3, [nondet]) :-
    named_expr_eval(mul(add(1, 2), 3), 9).
test(add_1_mul_2_3, [nondet]) :-
    named_expr_eval(add(1, mul(2, 3)), 7).
test(add_mul_1_2_add_3_4, [nondet]) :-
    named_expr_eval(add(mul(1, 2), mul(3, 4)), 14).
test(mul_add_1_2_mul_3_4, [nondet]) :-
    named_expr_eval(mul(add(1, 2), add(3, 4)), 21).

:-end_tests(named_expr_eval).

%%%%%%%%%%%%%%%%%%%%% named_expr_to_prefix_tokens/2 %%%%%%%%%%%%%%%%%%%%%

% #9: 15-points

% named_expr_to_prefix_tokens(NamedExpr, PrefixTokens): PrefixTokens is
% a list of the tokens in NamedExpr in prefix notation.
%
% *Hint*: use append/3.
% Base case: Convert a simple integer to a list containing just that integer.
named_expr_to_prefix_tokens(Expr, [Expr]) :- 
    integer(Expr).

% Recursive case for 'add' expressions.
named_expr_to_prefix_tokens(add(X, Y), [add | Tokens]) :-
    named_expr_to_prefix_tokens(X, TokensX),
    named_expr_to_prefix_tokens(Y, TokensY),
    append(TokensX, TokensY, Tokens).

% Recursive case for 'mul' expressions.
named_expr_to_prefix_tokens(mul(X, Y), [mul | Tokens]) :-
    named_expr_to_prefix_tokens(X, TokensX),
    named_expr_to_prefix_tokens(Y, TokensY),
    append(TokensX, TokensY, Tokens).

:-begin_tests(named_expr_to_prefix_tokens, []).
test(int, [nondet]) :-
    named_expr_to_prefix_tokens(42, [42]).

test(add_2_3, [nondet]) :-
    named_expr_to_prefix_tokens(add(2, 3), [add, 2, 3]).
test(add_add_2_3_add_4_5, [nondet]) :-
    named_expr_to_prefix_tokens(add(add(2, 3), add(4, 5)),
			 [add, add, 2, 3, add, 4, 5]).
test(add_add_add_add_1_2_3_4_5, [nondet]) :-
    named_expr_to_prefix_tokens(add(add(add(add(1, 2), 3), 4), 5),
			 [add, add, add, add, 1, 2, 3, 4, 5]).
test(add_add_add_add_1_2_3_4_5_fail, [fail]) :-
    named_expr_to_prefix_tokens(add(add(add(add(1, 2), 3), 4), 5), 
			 [add, add, add, 1, 2, 3, 4, 5]).
test(add_1_add_2_add_3_add_4_5, [nondet]) :-
    named_expr_to_prefix_tokens(add(1, add(2, add(3, add(4, 5)))),
			 [add, 1, add, 2, add, 3, add, 4, 5]).

test(mul_2_3, [nondet]) :-
    named_expr_to_prefix_tokens(mul(2, 3), [mul, 2, 3]).
test(mul_mul_2_3_mul_4_5, [nondet]) :-
    named_expr_to_prefix_tokens(mul(mul(2, 3), mul(4, 5)),
			 [mul, mul, 2, 3, mul, 4, 5]).
test(mul_mul_mul_mul_1_2_3_4_5, [nondet]) :-
    named_expr_to_prefix_tokens(mul(mul(mul(mul(1, 2), 3), 4), 5),
			 [mul, mul, mul, mul, 1, 2, 3, 4, 5]).
test(mul_mul_mul_mul_1_2_3_4_5_fail, [fail]) :-
    named_expr_to_prefix_tokens(mul(mul(mul(mul(1, 2), 3), 4), 5), 
			 [mul, mul, mul, 1, 2, 3, 4, 5]).
test(mul_1_mul_2_mul_3_mul_4_5, [nondet]) :-
    named_expr_to_prefix_tokens(mul(1, mul(2, mul(3, mul(4, 5)))),
			 [mul, 1, mul, 2, mul, 3, mul, 4, 5]).

test(mul_add_1_2_3, [nondet]) :-
    named_expr_to_prefix_tokens(mul(add(1, 2), 3), [mul, add, 1, 2, 3]).
test(add_1_mul_2_3, [nondet]) :-
    named_expr_to_prefix_tokens(add(1, mul(2, 3)), [add, 1, mul, 2, 3]).
test(add_mul_1_2_add_3_4, [nondet]) :-
    named_expr_to_prefix_tokens(add(mul(1, 2), mul(3, 4)),
			[add, mul, 1, 2, mul, 3, 4]).
test(mul_add_1_2_mul_3_4, [nondet]) :-
    named_expr_to_prefix_tokens(mul(add(1, 2), add(3, 4)),
			[mul, add, 1, 2, add, 3, 4]).
:-end_tests(named_expr_to_prefix_tokens).

%%%%%%%%%%%%%%%%%%%%%%% op_expr_to_prefix_expr/2 %%%%%%%%%%%%%%%%%%%%%%%

% #10: 10-points

% op_to_prefix_expr(OpExpr, PrefixTokens): Given a OpExpr involving
% integers, + and *, set PrefixTokens to a list containing its tokens
% in prefix notation.
%
% *Restriction*: must be implemented using *only* earlier procedures;
% cannot directly use recursion or Prolog built-ins.

% Transform operation expressions to prefix tokens
op_expr_to_prefix_tokens(Expr, [Expr]) :- 
    integer(Expr).

op_expr_to_prefix_tokens(+(X, Y), ['+'|PrefixTokens]) :-
    handle_expr(X, Y, PrefixTokens).

op_expr_to_prefix_tokens(*(X, Y), ['*'|PrefixTokens]) :-
    handle_expr(X, Y, PrefixTokens).

handle_expr(X, Y, CombinedTokens) :-
    op_expr_to_prefix_tokens(X, TokensX),
    op_expr_to_prefix_tokens(Y, TokensY),
    concat(TokensX, TokensY, CombinedTokens).

%  Concatenate two lists 
concat([], List, List).
concat([Head|Tail], List, [Head|ResultTail]) :-
    concat(Tail, List, ResultTail).

:-begin_tests(op_expr_to_prefix_tokens, []).
test(int, [nondet]) :-
    op_expr_to_prefix_tokens(42, [42]).

test(add_2_3, [nondet]) :-
    op_expr_to_prefix_tokens(+(2, 3), [+, 2, 3]).
test(add_add_2_3_add_4_5, [nondet]) :-
    op_expr_to_prefix_tokens(+(+(2, 3), +(4, 5)),
			 [+, +, 2, 3, +, 4, 5]).
test(add_add_add_add_1_2_3_4_5, [nondet]) :-
    op_expr_to_prefix_tokens(+(+(+(+(1, 2), 3), 4), 5),
			 [+, +, +, +, 1, 2, 3, 4, 5]).
test(add_add_add_add_1_2_3_4_5_fail, [fail]) :-
    op_expr_to_prefix_tokens(+(+(+(+(1, 2), 3), 4), 5), 
			 [+, +, +, 1, 2, 3, 4, 5]).
test(add_1_add_2_add_3_add_4_5, [nondet]) :-
    op_expr_to_prefix_tokens(+(1, +(2, +(3, +(4, 5)))),
			 [+, 1, +, 2, +, 3, +, 4, 5]).

test(mul_2_3, [nondet]) :-
    op_expr_to_prefix_tokens(*(2, 3), [*, 2, 3]).
test(mul_mul_2_3_mul_4_5, [nondet]) :-
    op_expr_to_prefix_tokens(*(*(2, 3), *(4, 5)),
			 [*, *, 2, 3, *, 4, 5]).
test(mul_mul_mul_mul_1_2_3_4_5, [nondet]) :-
    op_expr_to_prefix_tokens(*(*(*(*(1, 2), 3), 4), 5),
			 [*, *, *, *, 1, 2, 3, 4, 5]).
test(mul_mul_mul_mul_1_2_3_4_5_fail, [fail]) :-
    op_expr_to_prefix_tokens(*(*(*(*(1, 2), 3), 4), 5), 
			 [*, *, *, 1, 2, 3, 4, 5]).
test(mul_1_mul_2_mul_3_mul_4_5, [nondet]) :-
    op_expr_to_prefix_tokens(*(1, *(2, *(3, *(4, 5)))),
			 [*, 1, *, 2, *, 3, *, 4, 5]).

test(mul_add_1_2_3, [nondet]) :-
    op_expr_to_prefix_tokens(*(+(1, 2), 3), [*, +, 1, 2, 3]).
test(add_1_mul_2_3, [nondet]) :-
    op_expr_to_prefix_tokens(+(1, *(2, 3)), [+, 1, *, 2, 3]).
test(add_mul_1_2_add_3_4, [nondet]) :-
    op_expr_to_prefix_tokens(+(*(1, 2), *(3, 4)),
			[+, *, 1, 2, *, 3, 4]).
test(mul_add_1_2_mul_3_4, [nondet]) :-
    op_expr_to_prefix_tokens(*(+(1, 2), +(3, 4)),
			[*, +, 1, 2, +, 3, 4]).
:-end_tests(op_expr_to_prefix_tokens).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% main/0 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main :-
    current_prolog_flag(argv, Argv),
    (length(Argv, 0) -> run_tests ; run_tests(Argv)).

:-initialization(main, main).


    

  
