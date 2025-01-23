%%%%%%%%%%%%%%%%%%%%%%%%%
% ЦЕЛОЧИСЛЕНА АРИТМЕТИКА
%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(clpfd)).

% X #= Y
% X #\= Y
% X #> Y
% X #< Y
% X #>= Y
% X #=< Y
% X #<= Y - greshka!

% predikat za namirane na duljina na spisyk
% dulj(X N) - X e spisyk, N shte e duljinata mu
% tazi realizaciq nqma da raboti, zashtoto prolog ne e proektiran da raboti s mnogo strukturi
% i ako izwikame dulj([1,2,3], Y). shte poluchim Y = 0+1+1+1
% dulj([], 0).
% dulj([_|X], N+1) :- dulj(X, N).

%% N.B. Когато използваме челочислена аритметика, трябва да спазваме следното правило:
%% Във фактите, както и отляво на :- не бива да се използват аритметични изрази. По този
%% начин пролог "разбира" кои неща са числа и кои изрази.

duljina([], N) :- N #= 0.
duljina([_|X], N) :- N #>= 0, duljina(X, N-1).
% N #> 0 e kato dopulnitelna prowerka, za da moje da raboti predikata ako izwikame
% duljina(X, 3)., naprimer - toest, izwestno ni e kakwa e duljinata, no ne i kakwi
% elementi ima spisyka
% като цяло да знае колко да намаля стойността на N, за да има явно дефинирано дъно

% nestho dopylnitelno, s duljina([....], 2*K). ili duljina([....], 2*K+1).
% mojem da pitame dali spisyka e s chetna ili nechetna duljina

% definirame predikat za namirane na n-tiq element na spisyk X
% X - spisyk, N - poziciq, A - element
nth([A|_], N, A) :- N #= 1.
nth([_|X], N1, A) :- N1 #>= 1, N1 #= N + 1, nth(X, N, A).

% def. predikat za namirane na suma N na elementite na daden spisyk X
% ogr. otg. za broq na elementite na X
suma([], N) :- N #= 0.
suma([K|X], N) :- N #= M + K, suma(X, M).

% def. predikat za faktoriel
% ogr. otg. za N
fact(N, F) :- N #= 0, F #= 1.
fact(N1, F1) :- N1 #> 0, N1 #= N + 1, F1 #= N1 * F, fact(N, F).
% predikata moje da se izpolzwa i w obratna posoka
% fact(X, 120). => X = 5
% mojem dori da popitame koi chisla imat factoriel mejdu 10^100 i 10^101
% primer: ?- F #>= 10^100, F #=<10^101, fact(N,F).

% za aritmetichni urawneniq nqma algoritym kato algorityma za unifikaciq
% za termowete. W takuw sluchai, izpolzwame label([A1, .., An]), za da poiskame 
% komputera da generira konkretni stoinosti za A1, .., An.
% Ako ne izpolzwame label, komputera shte zapomni kakwo sme mu kazali za aritmetichnite
% urawneniq, shte naprawi burzi izwodi i do tam.
% label dawa poweche detaili i konkretika
% Primeri:
%% ?- N #> 0, N #< 4.
%% N in 1..3.

%% ?- N #> 0, N #< 4, label([N]).
%% N = 1 ;
%% N = 2 ;
%% N = 3.

% Предикатът label може да се използва само тогава, когато е известна
% крайна област, в която компютърът може да търси решение:
%% ?- N #> 0, label([N]).
%% ERROR: Arguments are not sufficiently instantiated

% Когато търси решения, пролог не винаги ще пробва всички възможно елементи
% на крайната област.  Да попитаме например дали има цели числа X, Y и Z,
% които са между 1 и 10000 и са решение на диофантовото уравнение
% (X+1000)³ - 2*(Y+1000)³ = Z

%% ?- 1#=<X,1#=<Y,1#=<Z,X#=<10000,Y#=<10000,Z#=<10000,
%%    (X+1000)^3 - 2*(Y+1000)^3 #= Z, label([X,Y,Z]).
%% false.

% Да забележим, че за всяка променлива има 10000 възможни стойности.
% Следователно вариациите на (X,Y,Z) са 1000000000000.  Това е е повече,
% отколкото компютърът може да провери за разумно време.  Въпреки това
% пролог намира отговора сравнително бързо, защото се сеща, че не е нужно
% да проверява всички вариации.

% Два ненужни предиката, които позволяват по-кратък запис:

% A in P..Q е еквивалентно на A #>= P, А #=< Q.
% [A1,A2,A3] ins P..Q е еквивалентно на A1 in P..Q, A2 in P..Q, A3 in P..Q.

% Докато при предикатите #>= и #=< отляво и отдясно може да стоят
% аритметични изрази, от дясната страна на предикатите in и ins не може да
% се използват изрази, а само променливи или константи.  Например не може
% да пишем K in 10^2..10^3.

% Zad. Da se napishe predikat limited_list(N, X), kojto po dadeno estestweno chislo N
% generira w X wsichki spisyci, chiqto dyljina e <= N i chiito elementi sa <= N
% Poqsnenie: tuk stawa duma za towa, che chrez preudowletworqwane (s ;) da generirame po edin spisyk, kojto da
% udowletworqwa uslowiqta na zadachata i da go zapiswame kato stoinost na X. Toest, X ne e spisyk ot spisyci
% limited_list(N, X) - X e spisyk s duljina <= N, chiito elementi sa
% estestweni chisla <= N
%
% Uslowie: izwestno e N

limited_list(N, X) :- L #=< N, duljina(X, L), X ins 0..N, label(X).

% primeren izhod izglejda taka
%% ?- limited_list(2,X).
%% X = [] ;
%% X = [0] ;
%% X = [1] ;
%% X = [2] ;
%% X = [0, 0] ;
%% X = [0, 1] ;
%% X = [0, 2] ;
%% X = [1, 0] ;
%% X = [1, 1] ;
%% X = [1, 2] ;
%% X = [2, 0] ;
%% X = [2, 1] ;
%% X = [2, 2] ;
%% false.

% moje da se naprawi i reshenie bez predikata "ins" po sledniq nachin

limited_list_2(N, X) :- L #=< N, duljina(X, L), elementi_ot_do(0, N, X), label(X).

% kato tuk trqbwa da definirame i elementi_ot_do kato predikat

% elementi_ot_do(N1, N2, X) - elementite na X sa mejdu N1 i N2 i sa celi, estestweni chisla
%
% Uslowie: izwestno e ogr. otg. za duljinata na X
elementi_ot_do(_, _, []).
elementi_ot_do(N1, N2, [A|X]) :- A #>= N1, A #=< N2, elementi_ot_do(N1, N2, X).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% БЕЗКРАЙНИ ГЕНЕРАТОРИ
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- use_module(library(clpfd)).

nat(N) :- N #= 0; nat(N-1).

% Пример 1.  Да се дефинира генератор на двойките [A,B] от естествени числа.
dwoika_est([A, B]) :- nat(N), A in 0..N, B in 0..N, label([A, B]).

% Пример 2. Да се дефинира генератор на всички списъци от естествени числа.
spisyk_est(X) :- nat(N), L #=< N, duljina(X, L), X ins 0..N, label(X).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% НЯКОИ ВЪЗМОЖНОСТИ НА ПРОЛОГ, КОИТО НИ ПОЗВОЛЯВАТ ДА ПИШЕМ КРАТКИ ПРОГРАМИ
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Тук ще разгледаме някои опасни възможности на пролог.  Те помагат да
% правим много кратки програми, но използването им е рисковано.  Ако
% използваме тези възможности неправилно, компютърът вместо да даде
% съобщение за грешка може да даде грешен отговор.

% ОТРИЦАНИЕ
% ---------
% not(p(x)) - naprimer

% Да дефинираме на един ред предикат, който проверява дали дадено число е просто.
%
% просто(P) - P е просто число
%
% Условие: известно е P
prosto(P) :- P #>= 2, not(( K #>= 2, K #< P, P #= K * M, label([K, M]) )).

% УНИВЕРСАЛЕН КВАНТОР С ИМПЛИКАЦИЯ
% --------------------------------

% Пример 1.  Да се дефинира предикат r(X,Y), който по дадени списъци X и Y
% проверява дали всеки елемент на X, който е четно число, е елемент на Y.

% r(X, Y) - всеки елемент на X, който е четно число е елемент на Y
%
% Условие: X и Y са известни списъци
r(X, Y) :- forall((element(A, X), A #= 2*B, label([B])), element(A, Y)).

% Ще дефинираме предикат за „подмножество“ по два начина: с импликация и с
% рекурсия.  Най-напред с импликация.
% подмножество1(X, Y) - елементите на X са елементи на Y
%
% Условие: X и Y са известни списъци
podmnojestwo1(X, Y) :- forall(element(A, X), element(A, Y)).

% подмножество2(X, Y) - елементите на X са елементи на Y
%
% Условие: известно е ограничение отгоре за броя на елементите на X и на Y
podmnojestwo2([], _).
podmnojestwo2([A|X], Y) :- element(A, Y), podmnojestwo2(X, Y).

% СПИСЪК ОТ ВСИЧКИ РЕШЕНИЯ
% ------------------------

% Задача.  Да се дефинира предикат ppp(X), който по даден списък от списъци
% X проверява дали сумата на елементите на четна позиция във всеки елемент
% на X, съдържащ просто число, e предпоследен елемент на елемент на X
% Най-напред от условието на задачата извличаме спецификацията на предиката:
% ppp(X) - сумата на елементите на четна позиция във всеки елемент на X,
%          съдържащ просто число, e предпоследен елемент на елемент на X
%
% Условие: X е известен списък
% Най-напред нека да изкажем условието на задачата по по-логически начин:
% Да се дефинира предикат ppp(X), който по даден списък от списъци
% X проверява дали е изпълнено следното свойство:
%
% за всяко Y ако
%      Y∈X и
%      ∃A(A∈Y и просто(А)) и
%      S е сумата на елементите на четна позиция в Y,
% то съществува Z, такова че
%      Z∈X и
%      S е предпоследен елемент на Z

ppp(X) :-
    forall( ( element(Y, X),
	      element(A, Y), prosto(A),
	      findall(B, (nth(Y, N, B), N #= 2*_), Y2), suma(Y2, S) ),
	    ( element(Z, X),
	      konkat(_, [S, _], Z) ) ).

% Задача.  Да се дефинира предикат pppp(X), който по даден списък X от
% списъци от числа проверява дали всеки предпоследен елемент на елемент на
% четна позиция е просто число
% Отново най-напред от условието на задачата извличаме спецификацията:
% pppp(X) - всеки предпоследен елемент на елемент на четна позиция в X е
%           просто число
%
% Условие: Известен е X.

pppp(X) :-
    % Тук X е известно, а за всички останали променливи има локални
    % квантори.
    forall(( % ∀K ∀A                            % за всеки K и A
             nth(X, 2*_K, A),                   % ако A е 2.К-тия елем. на X
             съединение(_, [Предпоследен,_], A) % и Предпоследен е предпо-
           ),                                   %   следният елем. на A
           просто(Предпоследен)).               % то Предпоследен e просто

% СОРТИРОВКА

sort_e(X, Y) :- permut1(X, Y), forall(konkat(_, [A, B|_], Y), A #=< B).

% Първа задача от писмения изпит по ЛП, януари 2023
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Списък от естествени числа [a_1, a_2,..., a_3n] ще наричаме
% специфичен, ако за всяко естествено число k ∈ {1,...,n} е изпълнено:
% 
%        ⎧ a_(a_k + 6),   ако 1 ≦ a_k + 6 ≦ 3n и 1 ≦ a_k ≦ k
% a_3k = ⎨
%        ⎩ 3a_(3n−k) + 1, иначе
%
% Да се дефинира предикат s(X),, който при преудовлетворяване генерира в X
% всички специфични списъци.

% Най-напред ще направим кратко решение „като за изпит“.  След това ще
% видим едно незначително по-дълго, но много по-ефективно решение.

% Краткото решение има следната структура:
%
% s1(X) :- генерираме_произволен_списък(X),
%          проверяваме_че_е_специфичен(X).

s1(X) :- nat(N), Dulj #=< N, duljina(X, Dulj), X ins 0..N, label(X), specifichen(X).

% специфичен(X) - X е специфичен
%
% Условие: X е известен списък

specifichen(X) :- duljina(X, 3*N),
                    forall( (K in 1..N, label([K])),
                            (nth(X, K, Ak), nth(X, 3*K, A3k), 
                            (1 #=< Ak+6, Ak+6 #=< 3*N, 1 #=< Ak, Ak #=< K -> nth(X, Ak+6, A3k) ; 
                                A3k #= 3*A3nk+1, nth(X, 3*N-K, A3nk))) ).

s2(X) :- nat(N), Dulj #=< N, duljina(X, Dulj), X ins 0..N, specifichen2(X), label(X).

specifichen2(X) :- duljina(X, 3*N), chastichno_specifichen(X, N).

chastichno_specifichen(_, K) :- K #= 0.
chastichno_specifichen(X, K) :- k #> 0, duljina(X, 3*N), nth(X, K, Ak), nth(X, 3*K, A3k),
                                (1 #=< Ak+6, Ak+6 #=< 3*N, 1 #=< Ak, Ak #=< K, nth(X, Ak+6, A3k)
                                ;
                                (1 #> Ak+6 #\/ Ak+6 #> 3*N #\/ 1 #> Ak #\/ Ak #> K), 
                                A3k #= 3*A3nk+1, nth(X, 3*N-K, A3nk)),
                                specifichen(X, K-1).