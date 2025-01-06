not_sorted(L) :- app(_, [A, B|_], L), A>B.
sorted(L) :- not(not_sorted(L)).

perm([], []).
perm([H|T], P) :- perm(T, PT), app(P1, P2, PT), app(P1, [H|P2], P).
sort1(L, S) :- perm(L, S), sorted(S).

% Този код гърми и не разбирам какво защо в бектрейса се показва лог и от функция, която не се извиква тук
lenPred([], 0).
lenPred([H|T], N) :- M is N-1, lenPred(T, M).

len_gen([], 0).
len_gen([H|T], N) :- len_gen(T, M), N is M+1.

