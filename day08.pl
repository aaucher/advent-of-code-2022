:- use_module(library(charsio)).
:- use_module(library(clpb)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(dif)).
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(library(reif)).

input_file('inputs/day08.txt').

eol --> "\n".

digit(D) --> [D], { char_type(D, decimal_digit) }.

tree(Height) --> digit(D), { number_chars(Height, [D]) }.

row([T|Ts]) --> tree(T), row(Ts).
row([]) --> eol.

map([R|Rs]) --> row(R), map(Rs).
map([R]) --> row(R).

input(Map, File) :-
	phrase_from_file(map(Map), File).

visible_tree_trees(Tree, Trees) :- maplist(#>(Tree), Trees).

visible(Tree, North, _, _, _, true) :- visible_tree_trees(Tree, North).
visible(Tree, _, South, _, _, true) :- visible_tree_trees(Tree, South).
visible(Tree, _, _, East, _, true) :- visible_tree_trees(Tree, East).
visible(Tree, _, _, _, West, true) :- visible_tree_trees(Tree, West).
visible(Tree, North, South, East, West, false) :-
	\+ visible_tree_trees(Tree, North),
	\+ visible_tree_trees(Tree, South),
	\+ visible_tree_trees(Tree, East),
	\+ visible_tree_trees(Tree, West).

state(S0, S), [S] --> [S0].
state(S), [S] --> [S].

prefix(I, Prefix, Ls) :-
	length(Prefix, I),
	append(Prefix, _, Ls).

suffix(I, Suffix, Ls) :-
	Lsuffix #= L - I - 1,
	length(Ls, L),
	length(Suffix, Lsuffix),
	append(_, Suffix, Ls).

%% Trees from X,Y towards compass points.
coord_map_nsew(X, Y, map(Rows, Cols), [North, South, East, West]) :-
	nth0(Y, Rows, Row),
	nth0(X, Cols, Col),
	prefix(X, Tesw, Row),
	suffix(X, East, Row),
	prefix(Y, Htron, Col),
	suffix(Y, South, Col),
	reverse(Htron, North),
	reverse(Tesw, West).

count_visible(Tree) -->
	state(s(X, Y, Map, Count0), s(X, Y, Map, Count)),
	{ coord_map_nsew(X, Y, Map, [North, South, East, West]),
	  if_(visible(Tree, North, South, East, West),
	      Count #= Count0 + 1,
	      Count #= Count0) }.

walk_trees([_|_]) -->
	state(s(X0, Y, Map, Acc), s(X, Y, Map, Acc)),
	{ X #= X0 + 1 }.
walk_trees([]) -->
	state(s(_, Y0, Map, Acc), s(0, Y, Map, Acc)),
	{ Y #= Y0 + 1 }.

walk_row([Tree|Trees], TreeWalkRel) -->
	call(TreeWalkRel, Tree),
	walk_trees(Trees),
	walk_row(Trees, TreeWalkRel).
walk_row([], _) --> [].

walk([Row|Rows], TreeWalkRel) --> walk_row(Row, TreeWalkRel), walk(Rows, TreeWalkRel).
walk([], _) --> [].

% Does not terminate after the first answer...
walk_all(Rows, TreeWalkRel, Acc0, Acc) :-
	transpose(Rows, Cols),
	phrase(walk(Rows, TreeWalkRel), [s(0, 0, map(Rows, Cols), Acc0)], [s(_, _, _, Acc)]).

%% How many trees are visible from outside the grid?
problemA(Result) :-
	input_file(File),
	input(Rows, File),
	walk_all(Rows, count_visible, 0, Result).

%% ?- problemA(Result).
%@    Result = 1708
%@ ;  ... .

tree_trees_n_tree_seen(Tree, [T|Trees], N) :-
	Tree #> T,
	N #= N0 + 1,
	tree_trees_n_tree_seen(Tree, Trees, N0).
tree_trees_n_tree_seen(Tree, [T|_], 1) :- Tree #=< T.
tree_trees_n_tree_seen(_, [], 0).

mul(A,B,R) :- R #= A * B.

tree_trees_compass_points_scenic_score(Tree, TreesCompassPoints, ScenicScore) :-
	maplist(tree_trees_n_tree_seen(Tree), TreesCompassPoints, Ns),
	foldl(mul, Ns, 1, ScenicScore).

calculate_scenic_scores(Tree) -->
	state(s(X, Y, Map, ScenicScores), s(X, Y, Map, [ScenicScore|ScenicScores])),
	{ coord_map_nsew(X, Y, Map, TreesCompassPoints),
	  tree_trees_compass_points_scenic_score(Tree, TreesCompassPoints, ScenicScore) }.

%% What is the highest scenic score possible for any tree?
problemB(Result) :-
	input_file(File),
	input(Rows, File),
	walk_all(Rows, calculate_scenic_scores, [], ScenicScores),
	list_max(ScenicScores, Result).

%% ?- problemB(Result).
%@    Result = 504000
%@ ;  ... .
