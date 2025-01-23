natural(0).
natural(X) :- natural(Y), X is Y+1.

%pairs(X, Y) :- natural(D), in(0, D, Y), X is D-Y.

in(N, M, K) :- N =< M, K = N.
in(N, M, K) :- N < M, N1 is N+1, in(N1, M, K).

not_prime(P) :- P > 1, N is P-1, in(2, N, Q), P mod Q =:= 0.
not_prime(P) :- P =< 1.

gen_primes(P) :- natural(P), not(not_prime(P)).

twin_primes(P, Q) :- gen_primes(P), Q is P+2, not(not_prime(Q)).

gen_pairs_fib(0, 1).
gen_pairs_fib(X, Y) :- gen_pairs_fib(Z, X), Y is X + Z.

gen_fib(F) :- gen_pairs_fib(F, _).