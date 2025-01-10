% Prolog exam, 2018

% Zad 1

% obshti predikati
nat(0).
nat(N) :- nat(M), N is M + 1.

between_e(A, B, A) :- A =< B.
between_e(A, B, C) :- A < B, A1 is A + 1, between_e(A1, B, C).

genKS(1, S, [S]).
genKS(K, S, [X1|R]) :- K > 1, K1 is K - 1, between_e(0, S, X1), S1 is S - X1, genKS(K1, S1, R).

% Reshenie na 1.1.
genArithProg(_, 0, _, []).
genArithProg(Current, N, Diff, [Current|Result]) :- N > 0, N1 is N - 1, Next is Current + Diff, genArithProg(Next, N1, Diff, Result).

isPrime(X) :- X > 1, N is X // 2, not(( between_e(2, N, Y), X mod Y =:= 0 )).

main([]).
main(L) :- nat(N), genKS(3, N, [Start, NumOfElem, Diff]), Diff > 0, NumOfElem > 0, genArithProg(Start, NumOfElem, Diff, L), not(( member(X, L), isPrime(X) )).

genGeomProg(_, 0, _, []).
genGeomProg(Current, N, Diff, [Current|Result]) :- 
                                N > 0, N1 is N - 1, Next is Current * Diff, genGeomProg(Next, N1, Diff, Result).

isSquare(X) :- N is X // 2, between_e(0, N, Y), Y * Y =:= X.

mainGeom([]).
mainGeom(L) :- nat(N), genKS(3, N, [Start, NumOfElem, Diff]), Start > 0, Diff > 0, NumOfElem > 0, 
                genGeomProg(Start, NumOfElem, Diff, L), not(( member(X, L), isSquare(X) )). % spored reshenieto ot izpita e not(isSquare(X))

% ne sym nnapylno obeden wyw wernostta na mainGeom

% Zad 2
% obshti predikati
append_e([], L2, L2).
append_e([H|T], L2, [H|R]) :- append_e(T, L2, R).

isSubsetOf(A, B) :- not(( member_e(X, A), not(member_e(X, B)) )).

member_e(X, L) :- append_e(_, [X|_], L).

is_list([]).
is_list([_|_]).

isListOfLists(L) :- not(( member_e(X, L), not(is_list(X)) )).

% I.1.
% 1
p1(L) :- member_e([], L).

% 2
p2(L) :- member_e(Y, L), member_e(Z, L), not(( isSubsetOf(Y, Z) )).

% 3
p3(L) :- member(Y, L), not(( member(Z, L), not(isSubsetOf(Z, Y)) )).

% 4
% p4(X) :- not(p3(X)).
p4(L) :- not(( member_e(Y, L), not(( member_e(Z, L), not(isSubsetOf(Z, Y)) )) )).

% I.2.