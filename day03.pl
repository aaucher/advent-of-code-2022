:- use_module(library(charsio)).
:- use_module(library(clpb)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(library(reif)).

input_file('inputs/day03.txt').

eol --> "\n".

item(Item) --> [Item], { char_type(Item, alnum) }.

items([I|Is]) --> item(I), items(Is).
items([I]) --> item(I).

rucksack(Items) --> items(Items), eol.

rucksacks([R|Rs]) --> rucksack(R), rucksacks(Rs).
rucksacks([R]) --> rucksack(R).

input(Rucksacks, File) :-
	phrase_from_file(rucksacks(Rucksacks), File).

half_half(Ls, Left, Right) :-
	append(Left, Right, Ls),
	same_length(Left, Right).

ascii_priority(Ascii, Priority) :-
	Ascii #>= 65,		% A, 27
	Ascii #=< 90,		% Z, 52
	Priority #= Ascii - 38.
ascii_priority(Ascii, Priority) :-
	Ascii #>= 97,		% A, 1
	Ascii #=< 122,		% Z, 26
	Priority #= Ascii - 96.

priority(Item, Priority) :-
	char_code(Item, Ascii),
	ascii_priority(Ascii, Priority).

common_item([A|As], Bs, Common) :-
	if_(memberd_t(A, Bs),
	    Common = A,
	    common_item(As, Bs, Common)).

common_items([], _, []).
common_items([Item|As], B, [Item|CommonItems]) :-
	memberchk(Item, B),
	common_items(As, B, CommonItems).
common_items([Item|As], B, CommonItems) :-
	\+ memberchk(Item, B),	% not
	common_items(As, B, CommonItems).

common_item_rucksack(R, Common) :-
	half_half(R, As, Bs),
	common_item(As, Bs, Common).

common_item_priorities(Rucksacks, Priorities) :-
	maplist(common_item_rucksack, Rucksacks, CommonItems),
	maplist(priority, CommonItems, Priorities).

%% What is the sum of the priorities of those item types?
problemA(Result) :-
	input_file(File),
	input(Rucksacks, File),
	common_item_priorities(Rucksacks, Priorities),
	sum_list(Priorities, Result).

%% ?- problemA(Result).
%@    Result = 7980
%@ ;  ... .

rucksack3_common_item(R, S, T, CommonItems) :-
	common_items(R, S, CommonItems0),
	common_items(CommonItems0, T, CommonItems).

rucksacks3_common_item([R,S,T|Rucksacks], [CommonItem|CommonItems]) :-
	rucksack3_common_item(R, S, T, [CommonItem|_]),
	rucksacks3_common_item(Rucksacks, CommonItems).
rucksacks3_common_item([], []).

%% Find the item type that corresponds to the badges of each three-Elf group.
%% What is the sum of the priorities of those item types?
problemB(Result) :-
	input_file(File),
	input(Rucksacks, File),
	rucksacks3_common_item(Rucksacks, CommonItems),
	maplist(priority, CommonItems, Priorities),
	sum_list(Priorities, Result).

%% ?- problemB(Result).
%@    Result = 2881
%@ ;  ... .
