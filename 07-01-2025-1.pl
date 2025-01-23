% = e za rawenstwo mejdu izrazi
% 1+2=X+Y, namira X i Y
% 1+2=X, dawa stoinost na X rawna na 1+2
% 1+2=1+X, dawa stoinostta na X da e 2

sbor(0, X, X).
sbor(s(X), Y, s(Z)) :- sbor(X, Y, Z).

dx(0, 0).
dx(1, 0).
dx(2, 0).
dx(3, 0).
dx(4, 0).

dx(x, 1).
dx(y, 0).
dx(z, 0).
dx(t, 0).
dx(u, 0).
dx(v, 0).
dx(w, 0).

dx(-F, -DF) :- dx(F, DF).
dx(F+G, DF+DG) :- dx(F, DF), dx(G, DG). 
dx(F-G, DF-DG) :- dx(F, DF), dx(G, DG).
dx(F*G, DF*G+DG*F) :- dx(F, DF), dx(G, DG).
dx(F/G, (DF*G-DG*F)/(G*G)) :- dx(F, DF), dx(G, DG).

dx(ln(F), DF/F) :- dx(F, DF).
dx(sin(F), cos(F)*DF) :- dx(F, DF).
dx(cos(F), -sin(F)*DF) :- dx(F, DF).

formula(0+F, F).
formula(F+0, F).
formula(0-F, -F).
formula(F-0, F).
formula(0*_F, 0).
formula(_F*0, 0).
formula(0/_F, 0).
formula(1*F, F).
formula(F*1, F).
formula(F/1, F).
formula(sin(0), 0).
formula(F+(G+H), (F+G)+H).
formula(F*(G*H), (F*G)*H).
formula(F*2, 2*F).
formula(F*3, 3*F).
formula(F*4, 4*F).
formula(X+(-Y), X-Y).
formula((-X)-Y, -(X+Y)).
formula(X-(-Y), X+Y).
formula((-X)*Y, -(X*Y)).
formula(X*(-Y), -(X*Y)).
formula((-X)/Y, -(X/Y)).
formula(X/(-Y), -(X/Y)).
formula(-(-X), X).

reduction(F, F1) :- formula(F, F1).
reduction(F+G, F1+G) :- reduction(F, F1).
reduction(G+F, G+F1) :- reduction(F, F1).
reduction(F-G, F1-G) :- reduction(F, F1).
reduction(G-F, G-F1) :- reduction(F, F1).
reduction(F*G, F1*G) :- reduction(F, F1).
reduction(G*F, G*F1) :- reduction(F, F1).
reduction(F/G, F1/G) :- reduction(F, F1).
reduction(G/F, G/F1) :- reduction(F, F1).
reduction(-F, -F1) :- reduction(F, F1).
reduction(ln(F), ln(F1)) :- reduction(F, F1).
reduction(sin(F), sin(F1)) :- reduction(F, F1).
reduction(cos(F), cos(F1)) :- reduction(F, F1).

simplification(F, G) :- (reduction(F, F1) -> simplification(F1, G) ; G = F).

%%%%%%%%%%%%%%%%%
% СПИСЪЦИ
%%%%%%%%%%%%%%%%%

% za spisycite, ako polzwame [A|B] kato notaciq i spisyka ima dyljina 5, naprimer
% moje da izpolzwame maksimum [A, B, C, D, Y|E]
% ako izpolzwame, naprimer, [A, B, C, D, Y, E|F] weche nadhrylqme duljinata na spisyka
% i poluchawame false

% Za kursa shte spazwame slednata ugoworka
% za spisyci: X, Y, Z
% za elementi: A, B, C
% za spisyk ot spisyci XX, YY, ZZ

% СТРУКТУРНА ИНДУКЦИЯ (РЕКУРСИЯ) ЗА СПИСЪЦИ:
%even_pos(X, Y) - Y elem na X, koito sa na chetna poziciq
even_pos([], []).
even_pos([_|X], Y) :- odd_pos(X, Y).

% odd_pos(X, Y) - Y sydyrja elem na X, koito sa na nechetni posicii
odd_pos([], []).
odd_pos([A|X], [A|Y]) :- even_pos(X, Y).

% ne sme definirali 2 funkcii
% prosto sme obqsnili na komputera kakwo oznachawa daden spisyk da sydyrja
% elementite na chetna poziciq ot drug daden spisyk

% oswen towa, komputera moje i da suzdade spisyk kato mu kajem
% even_pos(X, [2,4,6]), odd_pos(X, [1,3,5,7]). kato w X syzdawa spisyka [1,2,3,4,5,6,7]
% razbira se, mojem i da izwikame samo predicat even_pos ili odd_pos i da syzdadem
% nepylen spisyk, kojto ima stoinosti samo na chetnite ili na nechetnite pozicii

% keep in mind:
% [_|B]=[1,2,3,4]. => [2,3,4]
% [_,_|B]=[1,2,3,4]. => [3,4]

% dostatychno uslowie za korektnost na p tuk e da znaem dyljinata na X
% w protiwen sluchai, shte predizwikame greshka, zashtoto shte ni pokaje edinstweniq
% weren otgowor, no shte produlji da tyrsi i drugi, no drugi otgowori nqma
p(X, Y) :- even_pos(X, V), odd_pos(X, W), even_pos(Y, W), odd_pos(Y, V).

% imame go kato wraden predikat - member/2
element(A, [A|_]).
element(A, [_|X]) :- element(A, X).
% oswen, che prowerqwa dali daden element e chast ot daden spisyk,
% mojem da dadem elementa kato promenliwa i spysuk, kato prolog namira stoinosti
% na promenliwata


% mojem da namerim wsichi 3-elementni spisyci, naprimer, koito sysdyrjat a,b,c
% kato izpylnim X=[_, _, _], element(a, X), element(b, X), element(c, X).

% mojem syshto taka da naprawim konkatenaciq na dwa spisyk
% (nishto, che imame weche wgradena w prolog)

% nasha realizaciq na weche syshtestwuwashtiq append
konkat([], Y, Y).
konkat([A|X], Y, [A|Z]) :- konkat(X, Y, Z).

% workflow example
% [1,2,3], [4,5,6,7], Res -> [1|2,3], [4,5,6,7], [1|Z]
% [2,3], [4,5,6,7], Z -> [2|3], [4,5,6,7], [2|Z1]
% [3], [4,5,6,7], Z1 -> [3|[]] , [4,5,6,7], [3|Z2]
% [], [4,5,6,7], Z2 -> Z2=[4,5,6,7]
% [3|Z2] = [3|4,5,6,7] = [3,4,5,6,7]
% [2|Z1] = [2|3,4,5,6,7] = [2,3,4,5,6,7]
% [1|Z] = [1|2,3,4,5,6,7] = [1,2,3,4,5,6,7]

% syshto taka, raboti i po tozi nachin => konkat(X,[4,5,6,7], [1,2,2,2,3,4,5,6,7]).
% kakto i tozi => konkat([1,2,3,4,5],X, Y).

% taka ne moje da raboti => konkat(X, [1,2,3,4,5], Y).
% prichina: generira bezkraino i moje da zacikli, zashoto prolog generira wsichki
% wyzmojni resheniq

% moje da ni nameri kak moje da se razcepi daden spisyk, naprimer [1,2,3,4]
% konkat(X, Y, [1,2,3,4]).

% prediakta prowerqwa dali A e predposleden element na X
pre_last(X, A) :- konkat(_, [A,_], X).

% predikat prowerqwa dali spisyk X e nachalo na spisyk Y
prefix(X, Y) :- konkat(X, _, Y).

% mojem da pitame koi sa prefixite na spisyk
% prefix(X, [1,2,3,4]).
% momej da pitame i na koj spisyk e prefix [1,2,3], no otogwora shte e lakonichen
% prefix([1,2,3], X).
% Result: X = [1,2,3|_address]
% resultata e qsen, no ne e mnogo informatiwen

% pokazwa rotaciq na elementite na spisyk X w spisyk Y
% moje da generira w Y zawyrtaneto na spisyk X kato element 1 stawa posleden, 
% a wtoriq stawa pyrwi
% na wtoroto zawyrtane nowiqt pyrwi (pyrwonachalen wtori) element stawa posleden,
% a tekushtiq wtori, stawa pyrwi.
% Towa se powtarq dokato prwonachalniq pyrwi element ne zastane otnowo na pyrwa poziciq
rotation(X, Y) :- konkat(X1, X2, X), konkat(X2, X1, Y).

% interesen primer za upotrebata na rotation - za namirane na ladowe
% Генериране на музикалните ладове
%% ?- ротация([до,ре,ми,фа,сол,ла,си], X).
%% X = [до, ре, ми, фа, сол, ла, си] ;    % йонийски
%% X = [ре, ми, фа, сол, ла, си, до] ;    % дорийски
%% X = [ми, фа, сол, ла, си, до, ре] ;    % фригийски
%% X = [фа, сол, ла, си, до, ре, ми] ;    % лидийски
%% X = [сол, ла, си, до, ре, ми, фа] ;    % миксолидийски
%% X = [ла, си, до, ре, ми, фа, сол] ;    % еолийски
%% X = [си, до, ре, ми, фа, сол, ла] ;    % локрийски
%% X = [до, ре, ми, фа, сол, ла, си] ;    % йонийски
%% false.


% Ima dwa nachina za definirane na predikat za permutaciq na elem na daden spisyk:
% 1-wi nachin: структурна индукция по първия аргумент на предиката.
permut1([], []).
permut1([A|X], Z) :- permut1(X, Y), wmuk2(A, Y, Z).

% wmuk e predikat, koito wmukwa A na proizwolna poziciq w X
wmuk(A, X, [A|X]).
wmuk(A, [B|X], [B|Y]) :- wmuk(A, X, Y).
% a moje i wmuk da se modificira do wmuk2, koqto e po-ogranichena, no po-kratka
% i s pulna korektnost
wmuk2(A, X, Y) :- konkat(X1, X2, X), konkat(X1, [A|X2], Y).

% 2-ri nachin: структурна индукция по втория аргумент на предиката.
permut2([], []).
permut2(X, [A|Z]) :- izmuk(X, A, Y), permut2(Y, Z).

% izmuk e predikat, koito maha proiwolen element ot X i go zapiswa w Y
% strukturna indukciq:
izmuk([A|Y], A, Y).
izmuk([B|X], A, [B|Z]) :- izmuk(X, A, Z).

% [1,2,3,4], 3, Res => B = 1, X = [2,3,4], A = 3, Res = [1|Z]
% [2,3,4], 3, Z => B = 2, X = [3,4], A = 3, Z = [2|Z1]
% [3,4], 3, Z1 => A = 3, Y = [4]
% Z1 = Y = [4]
% Z = [2|Z1] = [2|[4]] = [2,4]
% Res = [1|Z] = [1|[2,4]] = [1,2,4]
% 
% 
%

% weche ni e izwestno kakwo e wmuk, ta izmuk e negowoto obratno deistwie
% izmuk(X, A, Y) :- wmuk(A, Y, X).



% 08.01.2025
%rawna_duljina(X, Y) - spisycite X i Y imat rawna duljina
% ogranichenie na duljinata na pone ediniq spisyk otgore
rawna_duljina([], []).
rawna_duljina([_|X], [_|Y]) :- rawna_duljina(X, Y).

% definirame predikat dwuposochna permutaciq
% kato pri izwikwane nqma znachenie dali shte dadem stoinost samo na X ili samo na Y,
% rezultata shte e wsichki wyzmojni permutacii na izwestniq ni spisyk
% sushto taka, moje da proweri dali spisyk Y e permutaciq na X i/ili obratnoto
dwuposochna_permut(X, Y) :- rawna_duljina(X, Y), permut1(X, Y).






















