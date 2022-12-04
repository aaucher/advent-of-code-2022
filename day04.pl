:- use_module(library(charsio)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(pio)).

input_file('inputs/day04.txt').

eol --> "\n".

nat(N) --> number_(Cs), { number_chars(N, Cs) }.

number_([D|Ds]) --> digit(D), number_(Ds).
number_([D])    --> digit(D).

digit(D) --> [D], { char_type(D, decimal_digit) }.

assignment(assignment(From, To)) --> nat(From), "-", nat(To).

pair(Left-Right) --> assignment(Left), ",", assignment(Right), eol.

pairs([P|Ps]) --> pair(P), pairs(Ps).
pairs([P]) --> pair(P).

input(Pairs, File) :-
	phrase_from_file(pairs(Pairs), File).

left_contains_right(assignment(Lf, Lt), assignment(Rf, Rt)) :-
	Lf #=< Rf,
	Lt #>= Rt.

contains(Left-Right, 1) :-
	left_contains_right(Left, Right)
	;	 left_contains_right(Right, Left).

contains(Left-Right, 0) :-
	\+ left_contains_right(Left, Right),
	\+ left_contains_right(Right, Left).

%% In how many assignment pairs does one range fully contain the other?
problemA(Result) :-
	input_file(File),
	input(Pairs, File),
	maplist(contains, Pairs, Contains),
	sum_list(Contains, Result).

%% ?- problemA(Result).
%@    Result = 534
%@ ;  ... .

left_overlaps_right(assignment(Lf, Lt), assignment(Rf, Rt)) :-
	Lt #=< Rt,
	Lt #>= Rf.

overlaps(Left-Right, 1) :-
	left_overlaps_right(Left, Right)
	;	 left_overlaps_right(Right, Left).

overlaps(Left-Right, 0) :-
	\+ left_overlaps_right(Left, Right),
	\+ left_overlaps_right(Right, Left).

%% In how many assignment pairs do the ranges overlap?
problemB(Result) :-
	input_file(File),
	input(Pairs, File),
	maplist(overlaps, Pairs, Contains),
	sum_list(Contains, Result).

%% ?- problemB(Result).
%@    Result = 841
%@ ;  ... .
