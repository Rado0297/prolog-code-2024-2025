% 19.11.2024г., Зиновиев

% предикат за символно диференциране
d(0, 0).
d(1, 0).
d(2, 0).
d(3, 0).
d(e, 0).
d(x, 1).
d(y, 0).
d(z, 0).
d(t, 0).
d(F+G1, DF+DG1) :- d(F,DF), d(G1, DG1).
d(F-G1, DF-DG1) :- d(F,DF), d(G1, DG1).
d(F*G1, DF*G1+F*DG1) :- d(F,DF), d(G1, DG1).
d(F/G1, (DF*G1-F*DG1)/(G1^2)) :- d(F,DF), d(G1, DG1).
d(F^2, 2*F*DF) :- d(F,DF).
d(sin(F), cos(F)*DF) :- d(F,DF).
d(cos(F), -sin(F)*DF) :- d(F,DF).
d(-F, -DF) :- d(F,DF).
d(lnF, DF/F) :- d(F,DF).
d(e^F, e^F*DF) :- d(F,DF).

% Отговори:
    % ?- d(sin(cos(x)), F).
    % F = cos(cos(x))*(-sin(x)*1).

    % ?- d(sin(x+x^2)/(x+y), F).
    % F = (cos(x+x^2)*(1+2*x*1)*(x+y)-sin(x+x^2)*(1+0))/(x+y)^2 .

    % ?- d(e^2, F).
    % F = 2*e*0 .

    % ?- d(e^2, F).
    % F = 2*e*0 ;
    % F = e^2*0.

% formula(F, G) - изразът F може да се опрости до G
formula(x+0, x).
formula(0+x, x).
formula(x*0, 0).
formula(0*x, 0).
formula(x*1, x).
formula(1*x, x).
formula(0/x, 0).
formula(x/1, x).
formula(x^0, 1).
formula(0^x, 1).
formula(1^x, 1).
formula(x+x, 2x).
formula(x*x, x^2).
formula(0+0, 0).
formula(x+(y+z), (x+y)+z).
formula(x*(y*z), (x*y)*z).
formula(x*(y+z), x*y+x*z).
formula((y+z)*x, y*x+z*x).
formula(-(x+y), (-x)+(-y)).
formula(-(-x), x).
formula(x+(-y), x-y).
formula((-x)+y, y-x).

% oprostqvane(F, G) - изразът G се получава от опростяването на F
% ednostupkovo_oprostqvane(F, G) - G е резултат след една стъпка на опростяване на F
ednostupkovo_oprostqvane(F, G) :- formula(F, G).
ednostupkovo_oprostqvane(F1+F2, G1+F2) :- ednostupkovo_oprostqvane(F1, G1).

oprostqvane(F, G) :- (ednostupkovo_oprostqvane(F, F1) -> oprostqvane(F1, G1) ; F=G1).

елемент(A, [A|_]).
елемент(A, [_|X]) :- елемент(A, X).
елемент2(A, [B|X]) :- A=B ; елемент2(A, X).