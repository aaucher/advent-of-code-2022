:- use_module(library(charsio)).
:- use_module(library(clpb)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(dif)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(pio)).
:- use_module(library(reif)).

input_file('inputs/day11.txt').

eol --> "\n".

nat(N) --> number_(Cs), { number_chars(N, Cs) }.

number_([D|Ds]) --> digit(D), number_(Ds).
number_([D])    --> digit(D).

digit(D) --> [D], { char_type(D, decimal_digit) }.

item(I) --> nat(I).

items([I|Is]) --> item(I), ", ", items(Is).
items([I]) --> item(I).

starting_items(Is) --> "  Starting items: ", items(Is), eol.

operand(old) --> "old".
operand(nat(N)) --> nat(N).

operator(+) --> "+".
operator(*) --> "*".

operation(operation(Op, A, B)) -->
	"  Operation: new = ",
	operand(A),
	" ",
	operator(Op),
	" ",
	operand(B),
	eol.

test(Divisor) --> "  Test: divisible by ", nat(Divisor), eol.

then(To) --> "    If true: throw to monkey ", nat(To), eol.
else(To) --> "    If false: throw to monkey ", nat(To), eol.

monkey(monkey(I, Items, Operation, Test, Then, Else)) -->
	"Monkey ", nat(I), ":", eol,
	starting_items(Items),
	operation(Operation),
	test(Test),
	then(Then),
	else(Else).

monkeys([M|Ms]) --> monkey(M), eol, monkeys(Ms).
monkeys([M]) --> monkey(M).

input(Monkeys, File) :-
	phrase_from_file(monkeys(Monkeys), File).

apply_op(*, A, B, Result) :- Result #= A * B.
apply_op(+, A, B, Result) :- Result #= A + B.

old_operation_new(Old, operation(Op, old, old), New) :-
	apply_op(Op, Old, Old, New).
old_operation_new(Old, operation(Op, old, nat(N)), New) :-
	apply_op(Op, Old, N, New).

worry_level_decreased(W0, W) :- W #= W0 div 3.

monkey_old_new_to(monkey(_, _, Operation, Test, Then, Else), Old, New, To) :-
	old_operation_new(Old, Operation, New0),
	worry_level_decreased(New0, New),
	if_(0 #= New rem Test,
	    To = Then,
	    To = Else).

replace([_|Ls], 0, X, [X|Ls]).
replace([L|Ls0], I0, X, [L|Ls]) :-
	I0 #> 0,
	I #= I0 - 1,
	replace(Ls0, I, X, Ls).

inspect([Item|Items], Monkey, ItemsPerMonkey0, ItemsPerMonkey) :-
	monkey_old_new_to(Monkey, Item, New, To),
	nth0(To, ItemsPerMonkey0, ItemsTo),
	replace(ItemsPerMonkey0, To, [New|ItemsTo], ItemsPerMonkey1),
	inspect(Items, Monkey, ItemsPerMonkey1, ItemsPerMonkey).
inspect([], Monkey, ItemsPerMonkey0, ItemsPerMonkey) :-
	monkey_i(Monkey, I),
	replace(ItemsPerMonkey0, I, [], ItemsPerMonkey).

state(S0, S), [S] --> [S0].

monkey_i(monkey(I, _, _, _, _, _), I).
monkey_starting_items(monkey(_, StartingItems, _, _, _, _), StartingItems).

%% ?- maplist('+', [1,2,3], [1,2,3]).
turn(Monkey) -->
	state(s(ItemsPerMonkey0, InspectedPerMonkey0), s(ItemsPerMonkey, InspectedPerMonkey)),
	{ monkey_i(Monkey, I),
	  nth0(I, ItemsPerMonkey0, Items),
	  reverse(Items, Smeti),
	  inspect(Smeti, Monkey, ItemsPerMonkey0, ItemsPerMonkey),
	  length(Items, NItems),
	  nth0(I, InspectedPerMonkey0, Inspected0),
	  Inspected #= Inspected0 + NItems,
	  replace(InspectedPerMonkey0, I, Inspected, InspectedPerMonkey) }.

round([Monkey|Monkeys]) --> turn(Monkey), round(Monkeys).
round([]) --> [].

simulate_round(Monkeys, _, S0, S) :- phrase(round(Monkeys), S0, S).

list_elem_times(Ls, E, N) :-
	length(Ls, N),
	maplist(=(E), Ls).

simulate(Monkeys, Nrounds, ItemsPerMonkey, Counts) :-
	maplist(monkey_starting_items, Monkeys, StartingItems),
	maplist(reverse, StartingItems, ItemsPerMonkey1),
	length(Rounds, Nrounds),
	length(Monkeys, Nmonkeys),
	list_elem_times(Counts0, 0, Nmonkeys),
	foldl(simulate_round(Monkeys), Rounds,
	      [s(ItemsPerMonkey1, Counts0)],
	      [s(ItemsPerMonkey, Counts)]).
simulate(Monkeys, StartingItems, Counts0, Nrounds, ItemsPerMonkey, Counts) :-
	maplist(reverse, StartingItems, ItemsPerMonkey1),
	length(Rounds, Nrounds),
	foldl(simulate_round(Monkeys), Rounds,
	      [s(ItemsPerMonkey1, Counts0)],
	      [s(ItemsPerMonkey, Counts)]).

monkey_business(InspectedPerMonkey0, MonkeyBusiness) :-
	pairs_keys(InspectedPerMonkeyPairs, InspectedPerMonkey0),
	keysort(InspectedPerMonkeyPairs, SortedAsc), % sort removes duplicates, ksort needed.
	reverse(SortedAsc, [First-_, Second-_|_]),
	MonkeyBusiness #= First * Second.

%% What is the level of monkey business after 20 rounds
%% of stuff-slinging simian shenanigans?
problemA(Result) :-
	input_file(File),
	input(Monkeys, File),
	simulate(Monkeys, 20, _, InspectedPerMonkey),
	monkey_business(InspectedPerMonkey, Result).

%% ?- problemA(Result).
%@    Result = 58786
%@ ;  ... .
