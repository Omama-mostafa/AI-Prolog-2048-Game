:- dynamic(boardScore/2).

ai(Depth):-
	generate(Board),
	showBoard(Board),
	aigame(Board, Depth, max).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%In the begin generate board with 2 random numbers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate(Board):-
	addNew([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],B),
	addNew(B,Board).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% To add new value :-
%	1- count number of Zeros
%	2- find random position in Zeros position
%	3- generate random number 2 or 4
%	4- replace random number with 0 in the random position
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
addNew(Board, NewBoard) :-
	countZeros(Board, Z),
	%write('------->'), write(Z),nl,
	checkZero(Z,L),
	position(L, P),
	twoOrFour(N),
	replaceZero(Board, P, N, NewBoard).

checkZero(Z,L):-
(Z == 0) -> (nl,write('No More Moves, AI Failed'),nl,abort);
L is Z.
countZeros([], 0).
countZeros([H|T], N) :-
	H == 0,
	countZeros(T, W),
	N is W + 1.
countZeros([H|T], N) :-
	H \= 0,
	countZeros(T, N).

position(NumberOfZeros, P) :-
    (NumberOfZeros == 16)->(write('enter initial position: '),read(N),P is N);
	P is random(NumberOfZeros).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 	generate random 2 or 4
% 	percentage for 2 is 90%
% 	persentage of 4 is 10%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

twoOrFour(N):-
	P is random(10),
	P < 9 -> N is 2;
	N is 4.

replaceZero([], _, _, []).
replaceZero([H1 | T1], P, N, [H2 | T2]):-
	H1 == 0,
	P == 0,
	H2 is N,
	replaceZero(T1, -1, N, T2).

% If greter than 0 go to another position (previous position)
replaceZero([H1 | T1], P, N, [H2 | T2]):-
	H1 == 0,
	P > 0,
	Pn is P - 1,
	H2 is H1,
	replaceZero(T1, Pn, N, T2).

% Already Replaced(be a negative value) in the first case after base case
replaceZero([H1 | T1], P, N, [H2 | T2]):-
	H1 == 0,
	P < 0,
	H2 is H1,
	replaceZero(T1, P, N, T2).

replaceZero([H1 | T1], P, N, [H2 | T2]):-
	H1 \= 0,
	H2 is H1,
	replaceZero(T1, P, N, T2).


showBoard([]).
showBoard([H | T]):-
	printNumber(H),
	showBoard1(T).

showBoard1([H | T]):-
	printNumber(H),
	showBoard2(T).

showBoard2([H | T]):-
	printNumber(H),
	showBoard3(T).

showBoard3([H | T]):-
	printNumber(H),nl,
	showBoard(T).

printNumber(N) :-
	N >= 1000,
	write(' '),write(N).
printNumber(N) :-
	N >= 100,
	write('  '),write(N).
printNumber(N) :-
	N >= 10,
	write('   '),write(N).
printNumber(N) :-
	N == 0,
	write('    _').
printNumber(N) :-
	write('    '),write(N).

% max list --> true if there is a max value in list = 2048
% otherwise --> false

aigame(Board, _, _):-
	max_list(Board, 2048),nl,
	write('Success!'), nl,
	abort.


aigame(Board, _, _):-
	noMoreMoves(Board),nl,
	write('Faild'), nl,
	abort.
	

aigame(Board, Depth, Turn):-
	write('Scores:'),

	moveLeft(Board, L) -> evaluate(Board, L, Depth, ScoreL),
	write('L = '),
	write(ScoreL),

	moveRight(Board, R) -> evaluate(Board, R, Depth, ScoreR),
	write('R = '),
	write(ScoreR),

	moveUp(Board, U) -> evaluate(Board, U, Depth, ScoreU),
	write('U = '),
	write(ScoreU),

	moveDown(Board, D) -> evaluate(Board, D, Depth, ScoreD),
	write('D = '),
	write(ScoreD),

	nextPlayer(Turn, NextTurn),
	selectMove(ScoreL, ScoreR, ScoreU, ScoreD, Move, NextTurn),
	aimove(Board, Move, NewBoard),
	showBoard(NewBoard),
	aigame(NewBoard, Depth, NextTurn).


aimove(Board, l, NewBoard) :-
	write(', Move: Left'),nl,
	moveLeft(Board, B) -> addNew(B, NewBoard).

aimove(Board, r, NewBoard) :-
	write(', Move: Right'),nl,
	moveRight(Board, B) -> addNew(B, NewBoard).

aimove(Board, d, NewBoard) :-
	write(', Move: Down'),nl,
	moveDown(Board, B) -> addNew(B, NewBoard).

aimove(Board, u, NewBoard) :-
	write(', Move: Up'),nl,
	moveUp(Board, B) -> addNew(B, NewBoard).

noMoreMoves(Board):-
	moveLeft(Board, X) -> equal(Board, X),
	moveRight(Board, Y) -> equal(Board, Y),
	moveUp(Board, Z) -> equal(Board, Z),
	moveDown(Board, W) -> equal(Board, W).

equal([], []).
equal([H1 | T1], [H2 | T2]):-
	H1 == H2,
	equal(T1, T2).

rotateRight([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4],[E1,E2,E3,E4,F1,F2,F3,F4,G1,G2,G3,G4,H1,H2,H3,H4]) :-
	E1 is D1,   E2 is C1,   E3 is B1,   E4 is A1,
	F1 is D2,   F2 is C2,   F3 is B2,   F4 is A2,
	G1 is D3,   G2 is C3,   G3 is B3,   G4 is A3,
	H1 is D4,   H2 is C4,   H3 is B4,   H4 is A4.

% rotate the board to the left
rotateLeft([A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4],[E1,E2,E3,E4,F1,F2,F3,F4,G1,G2,G3,G4,H1,H2,H3,H4]) :-
	E1 is A4,   E2 is B4,   E3 is C4,   E4 is D4,
	F1 is A3,   F2 is B3,   F3 is C3,   F4 is D3,
	G1 is A2,   G2 is B2,   G3 is C2,   G4 is D2,
	H1 is A1,   H2 is B1,   H3 is C1,   H4 is D1.

moveUp(Board, NewBoard):-
	rotateLeft(Board, Temp1),
	moveLeft(Temp1, Temp2),
	rotateRight(Temp2, NewBoard).

moveDown(Board, NewBoard):-
	rotateRight(Board, Temp1),
	moveLeft(Temp1, Temp2),
	rotateLeft(Temp2, NewBoard).

moveRight(Board, NewBoard):-
	rotateLeft(Board, Temp1),
	rotateLeft(Temp1, Temp2),
	moveLeft(Temp2, Temp3),
	rotateRight(Temp3, Temp4),
	rotateRight(Temp4, NewBoard).

moveLeft([], []).
% X|X|X|X ---> 2X|2X|0|0
moveLeft([X1, X2, X3, X4 | X], [N1, N2, N3, N4 | N]):-
	X1 \= 0,
	X3 \= 0,
	X1 = X2,
	X3 = X4,

	N1 is X1 + X2,
	N2 is X3 + X4,
	N3 = 0,
	N4 = 0,
	moveLeft(X, N).

% X|X|X|Y ---> 2X|X|Y|0
moveLeft([X1, X2, X3, X4 | X], [N1, N2, N3, N4 | N]):-
	X1 \= 0,
	X1 == X2,
	X2 == X3,

	N1 is X1 + X2,
	N2 is X3,
	N3 is X4,
	N4 is 0,
	moveLeft(X, N).

% X|X|Y|Z ---> 2X|Y|Z|0
moveLeft([X1, X2, X3, X4 | X], [N1, N2, N3, N4 | N]):-
	X1 \= 0,
	X1 == X2,
	X3 \= 0,

	N1 is X1 + X2,
	N2 is X3,
	N3 is X4,
	N4 is 0,
	moveLeft(X, N).

% X|Y|Z|Z ---> X|Y|2Z|0
moveLeft([X1, X2, X3, X4 | X], [N1, N2, N3, N4 | N]):-
	X1 \= 0,
	X2 \= 0,
	X1 \= X2,
	X2 \= X3,
	X3 == X4,

	N1 is X1,
	N2 is X2,
	N3 is X3 + X4,
	N4 is 0,
	moveLeft(X, N).

% X|Y|Y|Z ---> X|2Y|Z|0
moveLeft([X1, X2, X3, X4 | X], [N1, N2, N3, N4 | N]):-
	X1 \= 0,
	X2 \= 0,
	X2 == X3,

	N1 is X1,
	N2 is X2 + X3,
	N3 is X4,
	N4 is 0,
	moveLeft(X, N).

% 0|0|0|X ---> X|0|0|0
moveLeft([X1, X2, X3, X4 | X], [N1, N2, N3, N4 | N]):-
	X1 == 0,
	X2 == 0,
	X3 == 0,

	N1 is X4,
	N2 is 0,
	N3 is 0,
	N4 is 0,
	moveLeft(X, N).

% 0|0|X|X ---> 2X|0|0|0
moveLeft([X1, X2, X3, X4 | X], [N1, N2, N3, N4 | N]):-
	X1 == 0,
	X2 == 0,
	X3 == X4,

	N1 is X3 + X4,
	N2 is 0,
	N3 is 0,
	N4 is 0,
	moveLeft(X, N).

% 0|0|X|Y ---> X|Y|0|0
moveLeft([X1, X2, X3, X4 | X], [N1, N2, N3, N4 | N]):-
	X1 == 0,
	X2 == 0,

	N1 is X3,
	N2 is X4,
	N3 is 0,
	N4 is 0,
	moveLeft(X, N).

% 0|X|0|X ---> 2X|0|0|0
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]) :-
	X1 == 0,
	X3 == 0,
	X2 == X4,

	N1 is X2 + X4,
	N2 is 0,
	N3 is 0,
	N4 is 0,
	moveLeft(X,N).

% 0|X|0|Y ---> X|Y|0|0
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]) :-
	X1 == 0,
	X3 == 0,

	N1 is X2,
	N2 is X4,
	N3 is 0,
	N4 is 0,
	moveLeft(X,N).


% 0|X|X|Y -> 2X|Y|0|0
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]) :-
	X1 == 0,
	X2 == X3,

	N1 is X2 + X3,
	N2 is X4,
	N3 is 0,
	N4 is 0,
	moveLeft(X,N).

% 0|X|Y|Z -> X|Y|Z|0
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]) :-
	X1 == 0,
	X2 \= 0,
	X3 \= 0,
	X2 \= X3,
	X3 \= X4,

	N1 is X2,
	N2 is X3,
	N3 is X4,
	N4 is 0,
	moveLeft(X,N).

% 0|X|Y|Y -> X|2Y|0|0
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]) :-
	X1 == 0,
	X2 \= 0,
	X3 == X4,

	N1 is X2,
	N2 is X3 + X4,
	N3 is 0,
	N4 is 0,
	moveLeft(X,N).

% X|0|Y|Y -> X|2Y|0|0
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]) :-
	X1 \= 0,
	X2 == 0,
	X3 == X4,

	N1 is X1,
	N2 is X3 + X4,
	N3 is 0,
	N4 is 0,
	moveLeft(X,N).

% X|0|X|Y -> 2X|Y|0|0
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]) :-
	X1 \= 0,
	X2 == 0,
	X1 == X3,

	N1 is X1 + X3,
	N2 is X4,
	N3 is 0,
	N4 is 0,
	moveLeft(X,N).


% X|0|Y|Z -> X|Y|Z|0
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]) :-
	X1 \= 0,
	X2 == 0,
	X3 \= 0,

	N1 is X1,
	N2 is X3,
	N3 is X4,
	N4 is 0,
	moveLeft(X,N).


% X|0|0|X -> 2X|0|0|0
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]) :-
	X1 \= 0,
	X2 == 0,
	X3 == 0,
	X1 == X4,

	N1 is X1 + X4,
	N2 is 0,
	N3 is 0,
	N4 is 0,
	N4 is 0,
	moveLeft(X,N).


% X|0|0|Y -> X|Y|0|0
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]) :-
	X1 \= 0,
	X2 == 0,
	X3 == 0,

	N1 is X1,
	N2 is X4,
	N3 is 0,
	N4 is 0,
	moveLeft(X,N).

% X|X|0|Y -> 2X|Y|0|0
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]) :-
	X1 \= 0,
	X1 == X2,
	X3 == 0,

	N1 is X1 + X2,
	N2 is X4,
	N3 is 0,
	N4 is 0,
	moveLeft(X,N).

% X|Y|0|Y -> X|2Y|0|0
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]) :-
	X1 \= 0,
	X2 \= 0,
	X1 \= X2,
	X3 == 0,
	X2 == X4,

	N1 is X1,
	N2 is X2 + X4,
	N3 is 0,
	N4 is 0,
	moveLeft(X,N).

% X|Y|0|Z -> X|Y|Z|0
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]) :-
	X1 \= 0,
	X2 \= 0,
	X3 == 0,

	N1 is X1,
	N2 is X2,
	N3 is X4,
	N4 is 0,
	moveLeft(X,N).

% X|Y|Z|W -> X|Y|Z|W
moveLeft([X1,X2,X3,X4|X], [N1,N2,N3,N4|N]) :-
	X1 \= 0,
	X2 \= 0,
	X3 \= 0,

	N1 is X1,
	N2 is X2,
	N3 is X3,
	N4 is X4,
	moveLeft(X,N).


selectMove(ScoreL, ScoreR, ScoreU, ScoreD, l, Turn):-
	(Turn = max ->
	ScoreL >= ScoreR,
	ScoreL >= ScoreU,
	ScoreL >= ScoreD);
	(ScoreL =< ScoreR,
	ScoreL =< ScoreU,
	ScoreL =< ScoreD).

selectMove(ScoreL, ScoreR, ScoreU, ScoreD, r, Turn):-
	(Turn == max) -> 
	(ScoreR >= ScoreL,
	ScoreR >= ScoreU,
	ScoreR >= ScoreD);
	
	(Turn == min) ->
	(ScoreR =< ScoreL,
	ScoreR =< ScoreU,
	ScoreR =< ScoreD).

selectMove(ScoreL, ScoreR, ScoreU, ScoreD, u, Turn):-
	(Turn == max) ->
	(ScoreU >= ScoreL,
	ScoreU >= ScoreR,
	ScoreU >= ScoreD);
	(Turn == min) ->
	(ScoreU =< ScoreL,
	ScoreU =< ScoreR,
	ScoreU =< ScoreD).

selectMove(ScoreL, ScoreR, ScoreU, ScoreD, d, Turn):-
	(Turn == max) -> 
	(ScoreD >= ScoreL,
	ScoreD >= ScoreR,
	ScoreD >= ScoreU);

	(Turn == min) ->
	(ScoreD =< ScoreL,
	ScoreD =< ScoreR,
	ScoreD =< ScoreU).

evaluate(Board, NewBoard, _, 0):-
	equal(Board, NewBoard).

evaluate(_, Board, Level, Score):-
	Level >= 0,
	NewLevel is Level -1,
	boardScore(Board, BScore),
	asserta(boardScore(Board, BScore)),
	evalNext(Board, 0, NewLevel, S0),
	evalNext(Board, 1, NewLevel, S1),
	evalNext(Board, 2, NewLevel, S2),
	evalNext(Board, 3, NewLevel, S3),
	evalNext(Board, 4, NewLevel, S4),
	evalNext(Board, 5, NewLevel, S5),
	evalNext(Board, 6, NewLevel, S6),
	evalNext(Board, 7, NewLevel, S7),
	evalNext(Board, 8, NewLevel, S8),
	evalNext(Board, 9, NewLevel, S9),
	evalNext(Board, 10, NewLevel, S10),
	evalNext(Board, 11, NewLevel, S11),
	evalNext(Board, 12, NewLevel, S12),
	evalNext(Board, 13, NewLevel, S13),
	evalNext(Board, 14, NewLevel, S14),
	evalNext(Board, 15, NewLevel, S15),

	Score is 10*BScore+ S0+ S1+ S2+ S3+ S4+ S5+ S6+ S7+ S8+ S9+ S10+ S11+ S12+ S13+ S14+ S15.

evalNext([0, A2, A3, A4, B1, B2, B3, B4, C1, C2, C3, C4, D1, D2, D3, D4], 0, Level, Score):-
	Level >= 0,
	NewLevel is Level - 1,
	evalMoves([2, A2, A3, A4, B1, B2, B3, B4, C1, C2, C3, C4, D1, D2, D3, D4],
		[4, A2, A3, A4, B1, B2, B3, B4, C1, C2, C3, C4, D1, D2, D3, D4], NewLevel, Score).


evalNext([A1, 0, A3, A4, B1, B2, B3, B4, C1, C2no, C3, C4, D1, D2, D3, D4], 1, Level, Score):-
	Level >= 0,
	NewLevel is Level - 1,
	evalMoves([A1, 2, A3, A4, B1, B2, B3, B4, C1, C2, C3, C4, D1, D2, D3, D4],
		[A1, 4, A3, A4, B1, B2, B3, B4, C1, C2, C3, C4, D1, D2, D3, D4], NewLevel, Score).


evalNext([A1, A2, 0, A4, B1, B2, B3, B4, C1, C2, C3, C4, D1, D2, D3, D4], 2, Level, Score):-
	Level >= 0,
	NewLevel is Level - 1,
	evalMoves([A1, A2, 2, A4, B1, B2, B3, B4, C1, C2, C3, C4, D1, D2, D3, D4],
		[A1, A2, 4, A4, B1, B2, B3, B4, C1, C2, C3, C4, D1, D2, D3, D4], NewLevel, Score).


evalNext([A1, A2, A3, 0, B1, B2, B3, B4, C1, C2, C3, C4, D1, D2, D3, D4], 3, Level, Score):-
	Level >= 0,
	NewLevel is Level - 1,
	evalMoves([A1, A2, A3, 2, B1, B2, B3, B4, C1, C2, C3, C4, D1, D2, D3, D4],
		[A1, A2, A3, 4, B1, B2, B3, B4, C1, C2, C3, C4, D1, D2, D3, D4], NewLevel, Score).

evalNext([A1, A2, A3, A4, 0, B2, B3, B4, C1, C2, C3, C4, D1, D2, D3, D4], 4, Level, Score):-
	Level >= 0,
	NewLevel is Level - 1,
	evalMoves([A1, A2, A3, A4, 2, B2, B3, B4, C1, C2, C3, C4, D1, D2, D3, D4],
		[A1, A2, A3, A4, 4, B2, B3, B4, C1, C2, C3, C4, D1, D2, D3, D4], NewLevel, Score).


evalNext([A1, A2, A3, A4, B1, 0, B3, B4, C1, C2, C3, C4, D1, D2, D3, D4], 5, Level, Score):-
	Level >= 0,
	NewLevel is Level - 1,
	evalMoves([A1, A2, A3, A4, B1, 2, B3, B4, C1, C2, C3, C4, D1, D2, D3, D4],
		[A1, A2, A3, A4, B1, 4, B3, B4, C1, C2, C3, C4, D1, D2, D3, D4], NewLevel, Score).


evalNext([A1, A2, A3, A4, B1, B2, 0, B4, C1, C2, C3, C4, D1, D2, D3, D4], 6, Level, Score):-
	Level >= 0,
	NewLevel is Level - 1,
	evalMoves([A1, A2, A3, A4, B1, B2, 2, B4, C1, C2, C3, C4, D1, D2, D3, D4],
		[A1, A2, A3, A4, B1, B2, 4, B4, C1, C2, C3, C4, D1, D2, D3, D4], NewLevel, Score).


evalNext([A1, A2, A3, A4, B1, B2, B3, 0, C1, C2, C3, C4, D1, D2, D3, D4], 7, Level, Score):-
	Level >= 0,
	NewLevel is Level - 1,
	evalMoves([A1, A2, A3, A4, B1, B2, B3, 2, C1, C2, C3, C4, D1, D2, D3, D4],
		[A1, A2, A3, A4, B1, B2, B3, 4, C1, C2, C3, C4, D1, D2, D3, D4], NewLevel, Score).


evalNext([A1, A2, A3, A4, B1, B2, B3, B4, 0, C2, C3, C4, D1, D2, D3, D4], 8, Level, Score):-
	Level >= 0,
	NewLevel is Level - 1,
	evalMoves([A1, A2, A3, A4, B1, B2, B3, B4, 2, C2, C3, C4, D1, D2, D3, D4],
		[A1, A2, A3, A4, B1, B2, B3, B4, 4, C2, C3, C4, D1, D2, D3, D4], NewLevel, Score).


evalNext([A1, A2, A3, A4, B1, B2, B3, B4, C1, 0, C3, C4, D1, D2, D3, D4], 9, Level, Score):-
	Level >= 0,
	NewLevel is Level - 1,
	evalMoves([A1, A2, A3, A4, B1, B2, B3, B4, C1, 2, C3, C4, D1, D2, D3, D4],
		[A1, A2, A3, A4, B1, B2, B3, B4, C1, 4, C3, C4, D1, D2, D3, D4], NewLevel, Score).


evalNext([A1, A2, A3, A4, B1, B2, B3, B4, C1, C2, 0, C4, D1, D2, D3, D4], 10, Level, Score):-
	Level >= 0,
	NewLevel is Level - 1,
	evalMoves([A1, A2, A3, A4, B1, B2, B3, B4, C1, C2, 2, C4, D1, D2, D3, D4],
		[A1, A2, A3, A4, B1, B2, B3, B4, C1, C2, 4, C4, D1, D2, D3, D4], NewLevel, Score).


evalNext([A1, A2, A3, A4, B1, B2, B3, B4, C1, C2, C3, 0, D1, D2, D3, D4], 11, Level, Score):-
	Level >= 0,
	NewLevel is Level - 1,
	evalMoves([A1, A2, A3, A4, B1, B2, B3, B4, C1, C2, C3, 2, D1, D2, D3, D4],
		[A1, A2, A3, A4, B1, B2, B3, B4, C1, C2, C3, 4, D1, D2, D3, D4], NewLevel, Score).


evalNext([A1, A2, A3, A4, B1, B2, B3, B4, C1, C2, C3, C4, 0, D2, D3, D4], 12, Level, Score):-
	Level >= 0,
	NewLevel is Level - 1,
	evalMoves([A1, A2, A3, A4, B1, B2, B3, B4, C1, C2, C3, C4, 2, D2, D3, D4],
		[A1, A2, A3, A4, B1, B2, B3, B4, C1, C2, C3, C4, 4, D2, D3, D4], NewLevel, Score).


evalNext([A1, A2, A3, A4, B1, B2, B3, B4, C1, C2, C3, C4, D1, 0, D3, D4], 13, Level, Score):-
	Level >= 0,
	NewLevel is Level - 1,
	evalMoves([A1, A2, A3, A4, B1, B2, B3, B4, C1, C2, C3, C4, D1, 2, D3, D4],
		[A1, A2, A3, A4, B1, B2, B3, B4, C1, C2, C3, C4, D1, 4, D3, D4], NewLevel, Score).


evalNext([A1, A2, A3, A4, B1, B2, B3, B4, C1, C2, C3, C4, D1, D2, 0, D4], 14, Level, Score):-
	Level >= 0,
	NewLevel is Level - 1,
	evalMoves([A1, A2, A3, A4, B1, B2, B3, B4, C1, C2, C3, C4, D1, D2, 2, D4],
		[A1, A2, A3, A4, B1, B2, B3, B4, C1, C2, C3, C4, D1, D2, 4, D4], NewLevel, Score).


evalNext([A1, A2, A3, A4, B1, B2, B3, B4, C1, C2, C3, C4, D1, D2, D3, 0], 15, Level, Score):-
	Level >= 0,
	NewLevel is Level - 1,
	evalMoves([A1, A2, A3, A4, B1, B2, B3, B4, C1, C2, C3, C4, D1, D2, D3, 2],
		[A1, A2, A3, A4, B1, B2, B3, B4, C1, C2, C3, C4, D1, D2, D3, 4], NewLevel, Score).
evalNext(_, _, _, 0).


evalMoves(B2, B4, Level, Score):-
	moveLeft(B2, B2L) -> evaluate(B2, B2L, Level, S2L),
	moveLeft(B4, B4L) -> evaluate(B4, B4L, Level, S4L),
	moveRight(B2, B2R) -> evaluate(B2, B2R, Level, S2R),
	moveRight(B4, B4R) -> evaluate(B4, B4R, Level, S4R),
	moveUp(B2, B2U) -> evaluate(B2, B2U, Level, S2U),
	moveUp(B4, B4U) -> evaluate(B4, B4U, Level, S4U),
	moveDown(B2, B2D) -> evaluate(B2, B2D, Level, S2D),
	moveDown(B4, B4D) -> evaluate(B4, B4D, Level, S4D),
	Score is 9 * (S2L + S2R + S2U + S2D) + S4L + S4R + S4U + S4D.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Score of a board is sum of squared all values
%%%%%%%%%%%%%%%%%%%%%%%%%%%
boardScore(Board, Score):-
	squared(Board, Squared),
	sum_list(Squared, Score).

squared([], []).
squared([H1 | T1], [H2 | T2]):-
	H2 is H1 * H1,
	squared(T1 , T2).


nextPlayer(max, min).
nextPlayer(min, max).


%:- use_module(library(http/thread_httpd)).
%:- use_module(library(http/http_dispatch)).
%:- use_module(library(http/html_write)).


% URL handlers.
%:- http_handler('/', handle_request,[]).

% Request handlers.
%handle_request(_Request) :-
%    ai(2),
%    reply_html_page(
%        [],
%        []
%    ).

%server(Port) :-
%    http_server(http_dispatch, [port(Port)]).

%:- initialization(server(8000)).



