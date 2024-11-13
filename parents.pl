male(charlie).
male(dan).
male(edward).

female(alice).
female(betsy).
female(diana).

parent(albert, bob).
parent(albert, bill).
parent(albert, betsy).

parent(alice, bob).
parent(alice, bill).
parent(alice, betsy).

parent(bob, carl).
parent(bob, charlie).

get_grandchild :-
    parent(albert, X),
    parent(X, Y),
    write('Albert\'s grandchild is '),
    write(Y), nl.