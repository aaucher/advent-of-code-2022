:- use_module(library(charsio)).
:- use_module(library(clpb)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(dif)).
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(library(reif)).

input_file('inputs/day09.txt').

eol --> "\n".

nat(N) --> number_(Cs), { number_chars(N, Cs) }.

number_([D|Ds]) --> digit(D), number_(Ds).
number_([D])    --> digit(D).

digit(D) --> [D], { char_type(D, decimal_digit) }.

move(r) --> "R".
move(l) --> "L".
move(u) --> "U".
move(d) --> "D".

list_elem_times(Ls, E, N) :-
	length(Ls, N),
	maplist(=(E), Ls).

moves(Steps) -->
	move(M),
	" ",
	nat(Times),
	eol,
	{ list_elem_times(Steps, M, Times) }.

motion(Steps) --> motion_([], Steps0), { reverse(Steps0, Steps) }.

motion_(Steps0, Steps) -->
	moves(Steps1),
	motion_(Steps2, Steps),
	{ append(Steps1, Steps0, Steps2) }.
motion_(Steps0, Steps) -->
	moves(Steps1),
	{ append(Steps1, Steps0, Steps) }.

input(Steps, File) :-
	phrase_from_file(motion(Steps), File).

move_dx_dy(r, 1, 0).
move_dx_dy(l, -1, 0).
move_dx_dy(u, 0, 1).
move_dx_dy(d, 0, -1).

head0_move_head(k(X0,Y0), M, k(X,Y)) :- head0_move_head(X0, Y0, M, X, Y).
head0_move_head(X0, Y0, M, X, Y) :-
	move_dx_dy(M, Dx, Dy),
	X #= X0 + Dx,
	Y #= Y0 + Dy.

delta_dx_dy(Dx, Dy, 1, 1) :- Dx #>= 1, Dy #= 2.
delta_dx_dy(Dx, Dy, 1, 1) :- Dx #= 2, Dy #>= 1.
delta_dx_dy(Dx, Dy, 1, -1) :- Dx #= 2, Dy #=< -1.
delta_dx_dy(Dx, Dy, 1, -1) :- Dx #>= 1, Dy #= -2.
delta_dx_dy(Dx, Dy, -1, 1) :- Dx #= -2, Dy #>= 1.
delta_dx_dy(Dx, Dy, -1, 1) :- Dx #=< -1, Dy #= 2.
delta_dx_dy(Dx, Dy, -1, -1) :- Dx #= -2, Dy #=< -1.
delta_dx_dy(Dx, Dy, -1, -1) :- Dx #=< -1, Dy #= -2.
delta_dx_dy(Dx, Dy, 0, 1) :- Dx #= 0, Dy #= 2.
delta_dx_dy(Dx, Dy, 0, -1) :- Dx #= 0, Dy #= -2.
delta_dx_dy(Dx, Dy, 1, 0) :- Dx #= 2, Dy #= 0.
delta_dx_dy(Dx, Dy, -1, 0) :- Dx #= -2, Dy #= 0.
delta_dx_dy(Dx, Dy, 0, 0) :- Dx #=< 1, Dx #>= -1, Dy #=< 1, Dy #>= -1.

head_tail0_tail(Xh, Yh, Xt0, Yt0, Xt, Yt) :-
	Dx #= Xh - Xt0,
	Dy #= Yh - Yt0,
	delta_dx_dy(Dx, Dy, Mx, My),
	Xt #= Xt0 + Mx,
	Yt #= Yt0 + My.

rope0_move_rope([k(Xh,Yh),k(Xt0,Yt0)|Knots0], Move, [k(Xt,Yt)|Knots]) :-
	head_tail0_tail(Xh, Yh, Xt0, Yt0, Xt, Yt),
	rope0_move_rope([k(Xt,Yt)|Knots0], Move, Knots).
rope0_move_rope([_], _, []).

list_last([_|[L|Ls]], Last) :- list_last([L|Ls], Last).
list_last([L], L).

state(S0, S), [S] --> [S0].
state(S), [S] --> [S].

walk_move(Move) -->
	state(s([Head0|Knots0], Visited0), s([Head|Knots], [Last|Visited0])),
	{ head0_move_head(Head0, Move, Head),
	  rope0_move_rope([Head|Knots0], Move, Knots),
	  list_last(Knots, Last) }.

walk([Move|Moves]) -->
	walk_move(Move),
	walk(Moves).
walk([]) --> [].

moves_tail_visited(Moves, Rope, Visited) :-
	phrase(walk(Moves), [s(Rope, [])], [s(_, Visited)]).

tail_visited_distinct(Visited, N) :-
	sort(Visited, VisitedDistinct),
	length(VisitedDistinct, N).

%% How many positions does the tail of the rope visit at least once?
problem(Nknots, Result) :-
	input_file(File),
	input(Moves, File),
	list_elem_times(Rope, k(0,0), Nknots),
	moves_tail_visited(Moves, Rope, Visited),
	tail_visited_distinct(Visited, Result).

problemA(Result) :- problem(2, Result).
%% ?- problemA(Result).
%@    Result = 6391
%@ ;  ... .

problemB(Result) :- problem(10, Result).
%% ?- problemB(Result).
%@    Result = 2593
%@ ;  ... .
