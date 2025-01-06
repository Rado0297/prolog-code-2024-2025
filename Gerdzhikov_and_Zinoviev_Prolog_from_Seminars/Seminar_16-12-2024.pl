natural(0).
natural(X) :- natural(Y), X is Y+1.

%pairs(X, Y) :- natural(D), in(0, D, Y), X is D-Y.

