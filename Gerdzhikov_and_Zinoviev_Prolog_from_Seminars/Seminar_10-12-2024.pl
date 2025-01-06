nth([A|_], N, A) :- N #= 1.
nth([_|X], N, A) :- N #>= 1, nth(X, N-1, A).

prosto(N) :- N #\= 1, not((K #>= 0, K*_ #= N, K #\= 1, K #\= N, label([K]))).

podmnoj(X, Y) :- forall(member(A, X), member(A, Y)).

sooort(X, Y) :- permutation(X, Y), forall(concat(_, [A,B|_], Y), A #=< B).