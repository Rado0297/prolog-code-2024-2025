% Операции
calculate(+, X, Y, Result) :- Result is X + Y.
calculate(-, X, Y, Result) :- Result is X - Y.
calculate(*, X, Y, Result) :- Result is X * Y.
calculate(/, X, Y, Result) :- Y \= 0, Result is X / Y.  % Избягване на деление на 0

% Сравнения
compare(<, X, Y) :- X < Y.
% compare(=<, X, Y) :- X =< Y.
compare(>, X, Y) :- X > Y.
% compare(>=, X, Y) :- X >= Y.
compare_op(Op, X, Y) :-
    call(Op, X, Y).

% Проверка за равенство
check_equal(=, X, Y) :- X =:= Y.  % Точно равенство на числа
check_equal(\=, X, Y) :- X =\= Y. % Точно неравенство на числа
check_equal(eq, X, Y) :- X == Y.  % Равенство на термове
check_equal(neq, X, Y) :- X \== Y. % Неравенство на термове
