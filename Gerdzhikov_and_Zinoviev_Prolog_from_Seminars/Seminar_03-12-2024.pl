%use_module(library(clpfd)).

%?- X = 1+3.
%X = 1+3.

%?- X #= 1+3.
%X = 4.

%?- 2*N + 4 #= K, K #= 48.
%N = 22;
%K = 48.

dulj([], 0).
dulj([_|X], N+1) :- dulj(X, N).
% Resp: X = 0+1+1+1.

duljina([], N) :- N #= 0.
duljina([_|X], N) :- duljina(X, N-1).
% Resp: X = 3.

suma([], N) :- N #= 0.
suma([A|X], N) :- N #= A+M, suma(X, M).
suma([N|X], Suma) :- suma(X, Suma-N). % не ми е ясно този ред какво прави и защо е там

suma_polojitelni([], N) :- N #= 0.
suma_polojitelni([N|X], Suma) :- Suma #>=0, N #>= 0, suma_polojitelni(X, Suma-N).

faktoriel(N, F) :- N #= 0, F #= 1.
faktoriel(N, F) :- N #>= 0, F #= F1*N, N1 #= N-1, faktoriel(N1, F1).

% To be continue....
nat(N) :- N #>= 0; nat(N-1).
%nat(N) :- N #>= 0, indomain(N).

% Заради липсата на работещ nat(), останалите предикати от упр на 3 дек 2024, също не работят

%dwoiki_est([N1, N2]) :- nat(Suma), N1 #>= 0, N2 #>= 0, Suma #= N1+N2, label([N1, N2]).
dwoika_est([A,B]) :- nat(N), A in 0..N, B in 0..N, label([A,B]).