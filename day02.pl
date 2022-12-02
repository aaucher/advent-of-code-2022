:- use_module(library(charsio)).
:- use_module(library(clpb)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(pio)).

input_file('inputs/day02.txt').

eol --> "\n".
space --> " ".

opponent('A') --> "A".
opponent('B') --> "B".
opponent('C') --> "C".

player('X') --> "X".
player('Y') --> "Y".
player('Z') --> "Z".

round(O, P) --> opponent(O), space, player(P), eol.

rounds([O-P|Rs]) --> round(O, P), rounds(Rs).
rounds([]) --> "".

input(Rounds, File) :-
	phrase_from_file(rounds(Rounds), File).

shape_value('X', 1).
shape_value('Y', 2).
shape_value('Z', 3).

round_outcome('A', 'X', 3).
round_outcome('B', 'Y', 3).
round_outcome('C', 'Z', 3).

round_outcome('A', 'Y', 6).
round_outcome('B', 'Z', 6).
round_outcome('C', 'X', 6).

round_outcome('A', 'Z', 0).
round_outcome('B', 'X', 0).
round_outcome('C', 'Y', 0).

score_strategy_([O-P|Rs], Strategy, Score0, Score) :-
	call(Strategy, O, P, Score1),
	Score2 #= Score0 + Score1,
	score_strategy_(Rs, Strategy, Score2, Score).
score_strategy_([], _, Score, Score).

score_strategy(Rs, Strategy, Score) :-
	score_strategy_(Rs, Strategy, 0, Score).

methodA(O, P, Score) :-
	shape_value(P, Value),
	round_outcome(O, P, Score0),
	Score #= Score0 + Value.

problemA(Score) :-
	input_file(File),
	input(Rounds, File),
	score_strategy(Rounds, methodA, Score).

%%?- problemA(Score).
%@    Score = 9759
%@ ;  ... .

command('X', 0).
command('Y', 3).
command('Z', 6).

methodB(O, C, Score) :-
	command(C, Outcome),
	round_outcome(O, P, Outcome),
	shape_value(P, Value),
	Score #= Outcome + Value.

problemB(Score) :-
	input_file(File),
	input(Rounds, File),
	score_strategy(Rounds, methodB, Score).

%%?- problemB(Score).
%@    Score = 12429
%@ ;  ... .
