element(A, [A|_]).
element(A, [_|X]) :- element(A, X).

concat([], Y, Y).
concat([A|X], Y, [A|Z]) :- concat(X, Y, Z).

prefix(X, Y) :- concat(X, _, Y).

suffix(X, Y) :- concat(_, X, Y).

infix(X, Y) :- prefix(L, Y), suffix(X, L).
%infix(X, Y) :- suffix(L, Y), prefix(X, L).
%infix(X, Y) :- prefix(X, L), suffix(L, Y).

permutation([], []).
permutation([A|X], Z) :- permutation(X, Y), vmuk(A, Y, Z).

vmuk(A, [], [A]).
vmuk(A, [B|X], [A,B|X]).
vmuk(A, [B|X], [B|Y]) :- vmuk(A, X, Y).

permut([], []).
permut(X, [A|Y]) :- izmuk(A, X, Z), permut(Z, Y).

izmuk(A, X, Z) :- vmuk(A, Z, X).

eq_len([], []).
eq_len([_|X], [_|Y]) :- eq_len(X, Y).

% това ще е симетрична пермутация
perm(X, Y) :- eq_len(X, Y), permut(X, Y).

even([], []).
even([_|X], Y) :- odd(X, Y).

odd([], []).
odd([A|X], [A|Y]) :- even(X, Y).