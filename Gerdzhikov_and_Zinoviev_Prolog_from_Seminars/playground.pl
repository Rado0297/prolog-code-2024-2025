% 0 - символ за индивидна константа, а не число
% n+1 числото ще представяме с терма s(n)
% например, 3 ще представим с s(s(s(n)))

% sbor(N, M, K) - K е сбор на ест. числа N и M
sbor(0, X, X).
sbor(s(X), Y, s(Z)) :- sbor(X, Y, Z).

dx(0 ,0).
dx(1 ,0).
dx(2 ,0).
dx(3 ,0).
dx(e ,0).
dx(x, 1).
dx(y, 0).
dx(z ,0).
dx(t ,0).
dx(u ,0).
dx(v ,0).
dx(w ,0).
dx(F+G ,DF+DG) :- dx(F, DF), dx(G, DG).
dx(F-G, DF-DG) :- dx(F, DF), dx(G, DG).
dx(F*G, F*DG+G*DF) :- dx(F, DF), dx(G, DG).
dx(F/G, (DF*G-DG*F)/(G^2)) :- dx(F, DF), dx(G, DG).
dx(F^2, 2*F*DF) :- dx(F, DF).
dx(-F, -DF) :- dx(F, DF).
dx(sin(F), cos(F)*DF) :- dx(F, DF).
dx(cos(F), -sin(F)*DF) :- dx(F, DF).
dx(ln(F), DF/F) :- dx(F, DF).
dx(e^F, DF*(e^F)) :- dx(F, DF).

formula(X+0,X).
formula(0+X,X).
formula(X-0,X).
formula(0-X,-X).
formula(_X*0,0).
formula(0*_X,0).
formula(X*1,X).
formula(1*X,X).
formula(0/_,0).
formula(X/1,X).
formula(_^0,1).
formula(0^_,0).
formula(1^_,1).
formula(X^1,X).
formula(X+1,1+X).
formula(X+2,2+X).
formula(X+3,3+X).
formula(X+e,e+X).
formula(X*2,2*X).
formula(X*3,3*X).
formula(X*e,e*X).
formula(X+X,2*X).
formula(X-X,0).
formula((F+X)-X,F).
formula((F-X)+X,F).
formula((F+X)+X,F+2*X).
formula(X*X,X^2).
formula((F*X)*X,F*X^2).
formula(1+1,2).
formula(1+2,3).
formula(2+1,3).

formula(X+(Y+Z), (X+Y)+Z).
formula(X*(Y*Z), (X*Y)*Z).
formula(X*(Y+Z), X*Y+X*Z).
formula((Y+Z)*X, Y*X+Z*X).
formula(X*(Y-Z), X*Y-X*Z).
formula((Y-Z)*X, Y*X-Z*X).
formula(-(X+Y), (-X)+(-Y)).
formula(-(-X),X).
formula(X+(-Y),X-Y).
formula(X-(-Y),X+Y).
formula((-X)+Y,Y-X).
formula((-X)*Y,-(X*Y)).
formula(X*(-Y),-(X*Y)).
formula((-X)/Y,-(X/Y)).
formula(X/(-Y),-(X/Y)).
formula((F+G)^2,F^2+2*F*G+G^2).
formula((F-G)^2,F^2-2*F*G+G^2).
formula((F*G)^2,(F^2)*(G^2)).

one_step_simplify(F, G) :- formula(F, G).

one_step_simplify(F+H, G+H) :- one_step_simplify(F,G).
one_step_simplify(H+F, H+G) :- one_step_simplify(F,G).
one_step_simplify(F*H, G*H) :- one_step_simplify(F,G).
one_step_simplify(H*F, H*G) :- one_step_simplify(F,G).
one_step_simplify(F-H, G-H) :- one_step_simplify(F,G).
one_step_simplify(H-F, H-G) :- one_step_simplify(F,G).
one_step_simplify(F/H, G/H) :- one_step_simplify(F,G).
one_step_simplify(H/F, H/G) :- one_step_simplify(F,G).
one_step_simplify(H^F, H^G) :- one_step_simplify(F,G).
one_step_simplify(-F1, -G1) :- one_step_simplify(F1,G1).
one_step_simplify(ln(F1), ln(G1)) :- one_step_simplify(F1,G1).
one_step_simplify(sin(F1), sin(G1)) :- one_step_simplify(F1,G1).
one_step_simplify(cos(F1), cos(G1)) :- one_step_simplify(F1,G1).

simplify(F, G) :-
    ( one_step_simplify(F, F1)
      -> simplify(F1, G)
      ; F = G ).

% Да пробваме:

%% ?- simplify(1+0*x,F).
%% F = 1.

%% ?- simplify(1+0*x+0,F).
%% F = 1.

%% ?- simplify((1+0*x+0)*(1+0),F).
%% F = 1.

%% ?- simplify((1+0*x+0)*(1+0)+1,F).
%% F = 2. // expected
%% но хвърля стек овърфлоу

