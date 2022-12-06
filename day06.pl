:- use_module(library(charsio)).
:- use_module(library(clpb)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(dif)).
:- use_module(library(lists)).
:- use_module(library(pio)).

input_file('inputs/day06.txt').

eol --> "\n".
char(C) --> [C].

stream([C|Cs]) --> char(C), stream(Cs).
stream([C]) --> char(C), eol.

input(Stream, File) :-
	phrase_from_file(stream(Stream), File).

%% All elements of Ls are different than X.
all_different(_, []).
all_different(X, [L|Ls]) :- dif(X, L), all_different(X, Ls).

%% All elements of Ls are different.
all_different([]).
all_different([L|Ls]) :- all_different(L, Ls), all_different(Ls).

list_sublist_position(Ls, Lss, I) :-
	append(Prefix, Suffix, Ls),
	append(Lss, _, Suffix),
	length(Prefix, I).

marker(MarkerSize, Ls, Marker, Pos) :-
	length(Marker, MarkerSize),
	all_different(Marker),
	list_sublist_position(Ls, Marker, Pos0),
	Pos #= Pos0 + MarkerSize.

%% How many characters need to be processed before the first start-of-packet
%% marker is detected?
problemA(Result) :-
	input_file(File),
	input(Stream, File),
	marker(4, Stream, _, Result).

%% ?- problemA(Result).
%@    Result = 1598
%@ ;  ... .

%% How many characters need to be processed before the first start-of-message
%% marker is detected?
problemB(Result) :-
	input_file(File),
	input(Stream, File),
	marker(14, Stream, _, Result).

%% ?- problemB(Result).
%@    Result = 2414
%@ ;  ... .
