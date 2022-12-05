:- use_module(library(charsio)).
:- use_module(library(clpb)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(dif)).
:- use_module(library(lists)).
:- use_module(library(pio)).

input_file('inputs/day05.txt').

eol --> "\n".
space --> " ".

nat(N) --> number_(Cs), { number_chars(N, Cs) }.

number_([D|Ds]) --> digit(D), number_(Ds).
number_([D])    --> digit(D).

digit(D) --> [D], { char_type(D, decimal_digit) }.

crate(none) --> "   ".
crate(Crate) --> "[", [Crate] ,"]".

row([C|Cs]) --> crate(C), space, row(Cs).
row([C]) --> crate(C), eol.

rows([R|Rs]) --> row(R), rows(Rs).
rows([R]) --> row(R).

stacks(Stacks) --> rows(Crates), { transpose(Crates, Stacks) }.

label --> digit(_).

labels --> label, labels.
labels --> space, labels.
labels --> eol.

move(move(Ncrates, From, To)) -->
	"move ", nat(Ncrates), " from ", nat(From0), " to ", nat(To0), eol,
	{ From #= From0 - 1,
	  To #= To0 - 1 }.

moves([Move|Moves]) --> move(Move), moves(Moves).
moves([Move]) --> move(Move).

stack_stack_no_none([], []).
stack_stack_no_none([none|Crates0], Crates) :-
	stack_stack_no_none(Crates0, Crates).
stack_stack_no_none([Crate|Crates], [Crate|Crates]) :- dif(Crate, none).

stacks_moves(Stacks, Moves) -->
	stacks(Stacks0),
	space, labels, eol,
	moves(Moves),
	{ maplist(stack_stack_no_none, Stacks0, Stacks) }.

input(Stacks, Moves, File) :-
	phrase_from_file(stacks_moves(Stacks, Moves), File).

%% Base case from stackoverflow...
replace([_|Ls], 0, X, [X|Ls]).
replace([L|Ls0], I0, X, [L|Ls]) :-
	I0 #> 0,
	I #= I0 - 1,
	replace(Ls0, I, X, Ls).

one_by_one(From0, To0, Ncrates, From, To) :-
	length(MovedCrates, Ncrates),
	append(MovedCrates, From, From0),
	reverse(MovedCrates, SetarcDevom),
	append(SetarcDevom, To0, To).

multiple_at_once(From0, To0, Ncrates, From, To) :-
	length(MovedCrates, Ncrates),
	append(MovedCrates, From, From0),
	append(MovedCrates, To0, To).

move_stacks_moved(MoveRelation, move(Ncrates, Ifrom, Ito), Stacks0, Stacks) :-
	nth0(Ifrom, Stacks0, From0),
	nth0(Ito, Stacks0, To0),
	call(MoveRelation, From0, To0, Ncrates, From, To),
	replace(Stacks0, Ifrom, From, Stacks1),
	replace(Stacks1, Ito, To, Stacks).

top([], []).
top([[L|_]|Stacks], [L|Tops]) :- top(Stacks, Tops).

problem(MoveRelation, Result) :-
	input_file(File),
	input(Stacks0, Moves, File),
	foldl(move_stacks_moved(MoveRelation), Moves, Stacks0, Stacks),
	top(Stacks, Result).

%% After the rearrangement procedure completes,
%% what crate ends up on top of each stack?
problemA(Result) :- problem(one_by_one, Result).

%% ?- problemA(Result).
%@    Result = "QGTHFZBHV"
%@ ;  ... .

%% With the 9001 crate mover, after the rearrangement procedure completes,
%% what crate ends up on top of each stack?
problemB(Result) :- problem(multiple_at_once, Result).

%% ?- problemB(Result).
%@    Result = "MGDMPSZTM"
%@ ;  ... .
