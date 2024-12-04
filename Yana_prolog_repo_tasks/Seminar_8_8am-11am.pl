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
reverseCust([], []).
reverseCust([H|T], R) :- reverseCust(T, RT), append(RT, [H], R).

% [1,2,3,4,5,6,7]

% reverseCust([2,3,4,5,6,7], RT), append(RT, [1], R)      RT=[7,6,5,4,3,2],   R=[7,6,5,4,3,2,1]
% reverseCust([3,4,5,6,7], RT), append(RT, [2], R)        RT=[7,6,5,4,3],     R=[7,6,5,4,3,2]
% reverseCust([4,5,6,7], RT), append(RT, [3], R)          RT=[7,6,5,4],       R=[7,6,5,4,3]
% reverseCust([5,6,7], RT), append(RT, [4], R)            RT=[7,6,5],         R=[7,6,5,4]
% reverseCust([6,7], RT), append(RT, [5], R)              RT=[7,6],           R=[7,6,5]
% reverseCust([7], RT), append(RT, [6], R)                RT=[7],             R=[7,6]
% reverseCust([], RT), append(RT, [7], R)                 RT=[],              R=[7]
% reverseCust([], [])

% [X|L] S
% L [X|S]
% [] S
% reverse1(L, R) :- reserve1(L, [], R).
% reverse1([], S, S).
% reverse1([H|T], Stack, R) :- reverse1(T, [H|Stack], R).

% < , =< , > , >=
lessOrEqual(X, Y) :- X =< Y.

% isSorted(L) (forall x \in L) (forall y \in L after x) lessOrEqual(x, y)
isSorted([]).
isSorted([_]).
isSorted([X, Y|T]) :- lessOrEqual(X, Y), isSorted([Y|T]).

% isSorted1(L) (exists x \in L) (exists y \in L after x) lessOrEqual(x, y)
isSorted1(L) :- not(( append(_, [X, Y|_], L), not(lessOrEqual(X, Y)) )).


% AxF-> \forall x F
% ExF -> \exists x F
% ~F -> \neg F

% AxF |=| ~Ex~F
% |= AxF <-> ~Ex~F

% X списък от числа, Y списъци от числа
% p1(X, Y) <-> Има елемент на X, който е елем. на y
p1(X, Y) :- member(Z, X), member(T, Y), member(Z, T).
% p2(X, Y) <-> Има ел на Х, който е във всеки ел на Y.
p2(X, Y) :- member(Z, X), not(( member(T, Y), not(member(Z, T)) )).
% p3(X, Y) <-> Всеки ел на Х е в ел на Y. AzEtf |=| !Ez!Etf
p3(X, Y) :- not(( member(Z, X), not(( member(T, Y), member(Z, T) )) )).
% p4(X, Y) <-> Всеки ел на Х е във всеки ел на Y. AzAtf |=| !EzEt!f
p4(X, Y) :- not(( member(Z, X), member(T, Y), not(member(Z, T)) )).

% subsequence(L, SL) - подредица SL на списъка (подредица) L
subsequence([], []).
subsequence([_|T], R) :- subsequence(T, R).
subsequence([H|T], [H|R]) :- subsequence(T, R).

% HW: subsequence with append

% (Ax \in L1)(x \in L2) isSubset
isSubset(L1, L2) :- not(( member(X, L1), not(member(X, L2)) )).

% m(L, M) Да се генерират в M всички списъци, чиито елементи са елементи на L
% L=[a, b]; M=[[], [a], [a, a], [a, b, a, b, b, b]...]
m(_, []).
m(L, [H|T]) :- m(L, T), member(H, L).
% това генерира като натиснем ;
% м генерира елементи от пауърсета на Л. Като натискаме ; получаваме по един елемент от ппауърсета на Л.
% ако искаме да спрем отговорите, натискаме ентър и приключва с тру.

palindrome(L) :- reverseCust(L, L).

palindrome1([]).
palindrome1([_]).
palindrome1(L) :- append([H|Rest], [H], L), palindrome1(Rest).

% 7. union, intersection, difference, subset, equal
union(A, B, X) :- member(X, A); member(X, B).
intersection(A, B, X) :- member(X, A), member(X, B).
difference(A, B, X) :- member(X, A), not(member(X, B)).
equal(A, B) :- isSubset(A, B), isSubset(B, A).

% 6.IV.2013 г.
% Задача 2. Казваме, че списък X мажорира списък Y , ако
% всички елементи на X са елементи на Y . Да се дефинира
% на пролог предикат q3(L,M), който по даден списък от спи-
% съци L намира списък M, който съдържа всички елементи
% на L и в който никой елемент не се мажорира от елемент,
% намиращ се след него в списъка.
maj(X, Y) :- isSubset(X, Y).
q3(L, M) :- toSet(L, L1), permutation2(L1, M), not(( append(_, [X|T], M), member(Y, T), maj(Y, X) )).
toSet([], []).
toSet([H|T], [H|R]) :- toSet(T, R), not(member(H, R)).
toSet([H|T], R) :- toSet(T, R), member(H, R).

% 6.IV.2013 г.
% Задача 1. Да се дефинира на пролог предикат q2(L), който
% по даден списък от различни списъци L проверява дали
% всеки два различни елемента на L имат общ елемент, който
% не принадлежи на някой елемент на L.
q2(L) :- not(( remove(X, L, L1), remove(Y, L1, L2),
                not(( member(T, X), member(T, Y), remove(Z, L2, _), not(member(T, Z)) )) )).


% q2([[1,2,3],[3,4,5],[5,6,7],[1,3,7,8,9]]). - make a good example to execute and return true

% [1,2,3]
% [1,4,5]
% [9,4,7]
% [9,8,11]

% q2([[1,2,3], [1,4,5], [9,4,7]])....

% 6.IV.2013 г.
% Задача 1. Да се дефинира на пролог предикат q(L), който
% по даден списък от различни списъци L проверява дали
% в L съществуват два различни елемента, които имат общ
% елемент, който не принадлежи на никой друг елемент на L.
