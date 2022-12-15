:- use_module(library(charsio)).
:- use_module(library(clpb)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(dif)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(library(reif)).

input_file('inputs/day10.txt').

eol --> "\n".

integer(N) --> signed_integer(Cs), { number_chars(N, Cs) }.

signed_integer(['-'|Ds]) --> "-", number_(Ds).
signed_integer(Ds) --> number_(Ds).

number_([D|Ds]) --> digit(D), number_(Ds).
number_([D])    --> digit(D).

digit(D) --> [D], { char_type(D, decimal_digit) }.

instruction(addx(N)) --> "addx ", integer(N), eol.
instruction(noop) --> "noop", eol.

program([I|Is]) --> instruction(I), program(Is).
program([]) --> [].

input(Program, File) :-
	phrase_from_file(program(Program), File).

microop(addx(N), [noop, addx(N)]).
microop(noop, [noop]).

program_microops(Program, Microops) :-
	program_microops_(Program, [], Microops).

program_microops_([I|Is], Microops0, Microops) :-
	microop(I, Mu),
	reverse(Mu, Um),
	append(Um, Microops0, Microops1),
	program_microops_(Is, Microops1, Microops).
program_microops_([], Microops0, Microops) :-
	reverse(Microops0, Microops).

%% s(Cycle, X, Strengths).

state(S), [S] --> [S].
state(S0, S), [S] --> [S0].

increment -->
	state(s(Cycle0, X, S), s(Cycle, X, S)),
	{ Cycle #= Cycle0 + 1 }.

run(addx(N)) -->
	state(s(Cycle, X0, S), s(Cycle, X, S)),
	{ X #= X0 + N }.

run(noop) --> [].

record(Cycles) -->
	state(s(Cycle, X, Strengths), s(Cycle, X, [Strength|Strengths])),
	{ memberchk(Cycle, Cycles),
	  Strength #= X * Cycle }.

record(Cycles) -->
	state(s(Cycle, _, _)),
	{ \+ memberchk(Cycle, Cycles) }.

run([I|Is], Cycles) -->
	increment,
	run(I),
	record(Cycles),
	run(Is, Cycles).
run([], _) --> [].

run_program(Program, Len, Cycles, Strengths) :-
	program_microops(Program, Microops0),
	length(Microops, Len),
	append(Microops, _, Microops0),
	phrase(run(Microops, Cycles), [s(1, 1, [])], [s(_, _, Strengths)]).

%% Find the signal strength during the 20th, 60th, 100th, 140th, 180th,
%% and 220th cycles. What is the sum of these six signal strengths?
problemA(Result) :-
	input_file(File),
	input(Program, File),
	run_program(Program, 220, [20, 60, 100, 140, 180, 220], Strengths),
	sum_list(Strengths, Result).

%% ?- problemA(Result).
%@    Result = 12460
%@ ;  ... .

draw_pixel -->
	state(s(Cycle, X, Outs), s(Cycle, X, ['#'|Outs])),
	{ Px #= Cycle rem 40,
	  Px #>= X - 1,
	  Px #=< X + 1 }.
draw_pixel -->
	state(s(Cycle, X, Outs), s(Cycle, X, ['.'|Outs])),
	{ Px #= Cycle rem 40,
	  (   Px #< X - 1
	  ;   Px #> X + 1) }.

newline -->
	state(s(Cycle, X, Outs), s(Cycle, X, ['\n'|Outs])),
	{ 39 #= Cycle rem 40 }.

newline -->
	state(s(Cycle, _, _)),
	{ 39 #\= Cycle rem 40 }.

draw([I|Is]) -->
	draw_pixel,
	newline,
	increment,
	run(I),
	draw(Is).
draw([]) --> [].

run_program_draw(Program, Len, Out) :-
	program_microops(Program, Microops0),
	length(Microops, Len),
	append(Microops, _, Microops0),
	phrase(draw(Microops), [s(0, 1, [])], [s(_, _, Out)]).

%% Render the image given by your program.
%% What eight capital letters appear on your CRT?
problemB :-
	input_file(File),
	input(Program, File),
	run_program_draw(Program, 240, Out0),
	reverse(Out0, Out),
	format("~s", [Out]).

%% ?- problemB.
%@ ####.####.####.###..###...##..#..#.#....
%@ #.......#.#....#..#.#..#.#..#.#.#..#....
%@ ###....#..###..#..#.#..#.#..#.##...#....
%@ #.....#...#....###..###..####.#.#..#....
%@ #....#....#....#....#.#..#..#.#.#..#....
%@ ####.####.#....#....#..#.#..#.#..#.####.
%@    true
%@ ;  ... .
