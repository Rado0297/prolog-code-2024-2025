natural(0).
natural(X) :- natural(Y), X is Y+1.

p(az, ti).
p(ti, toi).
p(ti, tq).

p1([]).
q1([_]).
r1([_|_]).

%индуктивна дефиниция на естествените числа
nat(c).
nat(s(X)) :- nat(X).

% Zad 6.1.
%sum(N, M, S) :- S is N+M.
% Генеративно решение на 6.1.
:- use_module(library(clpfd)).
sum(N, M, S) :- N in inf..S, M in inf..S, S #= N + M, label([N, M]).
% трябваше да се зададе интервал на стойностите на N и M

% Zad 7.1.
fib(0, 0).
fib(1, 1).
fib(N, F) :- N > 1, N1 is N-1, N2 is N-2, fib(N1, F1), fib(N2, F2), F is F1+F2.

% генератор на Фиб - нещо не заработи
%fib_clp(0, 0).
%fib_clp(1, 1).
%fib_clp(N, F) :- N #> 1, N1 #= N-1, N2 #= N-2, fib_clp(N1, F1), fib_clp(N2, F2), F #= F1+F2.

% Zad 7.2.
fact(0, 1).
fact(1, 1).
fact(N, F) :- N > 1, Prev is N-1, fact(Prev, R), F is R*N.

% Zad 7.3.
gcd(0, X, X) :- X > 0, !.
gcd(X, Y, Z) :- X >= Y, X1 is X-Y, gcd(X1, Y, Z).
gcd(X, Y, Z) :- X < Y, X1 is Y-X, gcd(X1, Y, Z).