% HW removeAll(X, List, Rlist), reverse(List, Rlist), permutation(List, Plist)

% премахване на всички срещания на елемент X от списък
removeAll(_, [], []).
removeAll(X, [X|T], R) :- removeAll(X, T, R).
removeAll(X, [H|T], [H|R]) :- X \= H, removeAll(X, T, R).

% permutation(List, PList)
remove(X, List, ResultList) :- append(A, [X|B], List), append(A, B, ResultList).

permutation([], []).
permutation(L, [X|P]) :- remove(X, L, M), permutation(M, P).

insert(X, List, ResultList) :- append(A, B, List), append(A, [X|B], ResultList).
permutation2([], []).
permutation2([H|T], R) :- permutation2(T, Q), insert(H, Q, R).

% reverse(List, RList)
reverse([], []),
reverse([H|T], R) :- reverse(T, RT), append(RT, [H], R).

% [1,2,3,4,5,6,7]

% reverse([2,3,4,5,6,7], RT), append(RT, [1], R)      RT=[7,6,5,4,3,2],   R=[7,6,5,4,3,2,1]
% reverse([3,4,5,6,7], RT), append(RT, [2], R)        RT=[7,6,5,4,3],     R=[7,6,5,4,3,2]
% reverse([4,5,6,7], RT), append(RT, [3], R)          RT=[7,6,5,4],       R=[7,6,5,4,3]
% reverse([5,6,7], RT), append(RT, [4], R)            RT=[7,6,5],         R=[7,6,5,4]
% reverse([6,7], RT), append(RT, [5], R)              RT=[7,6],           R=[7,6,5]
% reverse([7], RT), append(RT, [6], R)                RT=[7],             R=[7,6]
% reverse([], RT), append(RT, [7], R)                 RT=[],              R=[7]
% reverse([], [])