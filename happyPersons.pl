happy(albert).
happy(alice).
happy(bob).
happy(bill).
with_albert(alice).

runs(albert) :- happy(albert).

dances(alice) :-
    happy(alice),
    with_albert(alice).

does_alice_dance :-
    dances(alice),
    write('When Alice is happy and with Albert she dances').

near_water(bob).

swims(bob) :-
    happy(bob),
    near_water(bob).
