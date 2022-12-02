:- use_module(library(charsio)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(pio)).

input_file('inputs/day01.txt').

eol --> "\n".

nat(N) --> number_(Cs), { number_chars(N, Cs) }.

number_([D|Ds]) --> digit(D), number_(Ds).
number_([D])    --> digit(D).

digit(D) --> [D], { char_type(D, decimal_digit) }.

item_calories(C) --> nat(C).

elf_calories([C|Cs]) --> item_calories(C), eol, elf_calories(Cs).
elf_calories([C]) --> item_calories(C), eol.

elves([E|Es]) --> elf_calories(E), eol, elves(Es).
elves([E]) --> elf_calories(E).

input(Elves, File) :-
	phrase_from_file(elves(Elves), File).

elves_sum_calories(Elves, Sums) :-
	maplist(sum_list, Elves, Sums).

list_max([N|Ns], Max) :-
	foldl(list_max_, Ns, N, Max).

list_max_(N, Max0, Max) :-
	Max is max(N, Max0).

%% Find the Elf carrying the most Calories.
%% How many total Calories is that Elf carrying?
problemA(Max) :-
	input_file(File),
	input(Elves, File),
	elves_sum_calories(Elves, Sums),
	list_max(Sums, Max).

%?- problemA(Max).
%@    Max = 66719
%@ ;  ... .

%% Find the top three Elves carrying the most Calories.
%% How many Calories are those Elves carrying in total?
problemB(Sum3) :-
	input_file(File),
	input(Elves, File),
	elves_sum_calories(Elves, Sums0),
	sort(Sums0, Sums),
	reverse(Sums, [F,S,T|_]),
	Sum3 #= F + S + T.

%?- problemB(Sum3).
%@    Sum3 = 198551
%@ ;  ... .
