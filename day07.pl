:- use_module(library(charsio)).
:- use_module(library(clpb)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(dif)).
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(library(reif)).

input_file('inputs/day07.txt').

eol --> "\n".

nat(N) --> number_(Cs), { number_chars(N, Cs) }.

number_([D|Ds]) --> digit(D), number_(Ds).
number_([D])    --> digit(D).

digit(D) --> [D], { char_type(D, decimal_digit) }.

char(C) --> [C], { char_type(C, alpha) }.

chars([C|Cs]) --> char(C), ".", chars(Cs).
chars([C|Cs]) --> char(C), chars(Cs).
chars([C]) --> char(C).

dir(root) --> "/".
dir(parent) --> "..".
dir(dir(Dir)) --> chars(Dir).

file(file(File, Size)) --> nat(Size), " ", chars(File).

ls_output_line(File) --> file(File), eol.
ls_output_line(Dir) --> "dir ", dir(Dir), eol.

ls_output([L|Ls]) --> ls_output_line(L), ls_output(Ls).
ls_output([L]) --> ls_output_line(L).

command(cd(Dir)) --> "$ cd ", dir(Dir), eol.
command(ls(Files)) --> "$ ls", eol, ls_output(Files).

commands([C|Cs]) --> command(C), commands(Cs).
commands([C]) --> command(C).

input(Commands, File) :-
	phrase_from_file(commands(Commands), File).

state(S0, S), [S] --> [S0].
state(S), [S] --> [S].

%% state(Path, Tree)

walk_cd(cd(dir(Dir))) --> state(s(Path, Tree), s([Dir|Path], Tree)).
walk_cd(cd(parent)) --> state(s([_|Path], Tree), s(Path, Tree)).
walk_cd(cd(root)) --> state(s(_, Tree), s([], Tree)).

walk_ls(ls(Files)) -->
	state(s(Path, Tree), s(Path, [Path-Files|Tree])).

walk([C|Cs]) --> walk_cd(C), walk(Cs).
walk([C|Cs]) --> walk_ls(C), walk(Cs).
walk([]) --> [].

commands_tree(Commands, Tree) :-
	phrase(walk(Commands), [s([], [])], [s(_, Tree)]).

tree_path_files([Path-Files|_], Path, Files).
tree_path_files([Current-_|Tree], Path, Files) :-
	dif(Current, Path),
	tree_path_files(Tree, Path, Files).

size(file(_, Size), _, _, Size).
size(dir(Dir), Path, Tree, Size) :-
	tree_path_files(Tree, [Dir|Path], Files),
	size(Files, [Dir|Path], Tree, Size).
size([F|Fs], Path, Tree, Sum) :-
	Sum #= Sf + Sfs,
	size(F, Path, Tree, Sf),
	size(Fs, Path, Tree, Sfs).
size([], _, _, 0).

tree_path_sizes([Path-Files|Paths], Tree, [Path-Size|Sizes]) :-
	size(Files, Path, Tree, Size),
	tree_path_sizes(Paths, Tree, Sizes).
tree_path_sizes([], _, []).

size_below(Threshold, Size, true) :- Size #=< Threshold.
size_below(Threshold, Size, false) :- Size #> Threshold.

size_above(Threshold, Size, true) :- Size #>= Threshold.
size_above(Threshold, Size, false) :- Size #< Threshold.

path_size_size(_-Size, Size).

%% Find all of the directories with a total size of at most 100000.
%% What is the sum of the total sizes of those directories?
problemA(Result) :-
	input_file(File),
	input(Commands, File),
	commands_tree(Commands, Tree),
	tree_path_sizes(Tree, Tree, PathSizes),
	maplist(path_size_size, PathSizes, Sizes0),
	tfilter(size_below(100000), Sizes0, Sizes),
	sum_list(Sizes, Result).

%% ?- problemA(Result).
%@    Result = 1077191
%@ ;  false.

list_min([N|Ns], Min) :-
    foldl(list_min_, Ns, N, Min).

list_min_(N, Min0, Min) :-
    Min #= min(N, Min0).

free_threshold(PathSizes, Threshold) :-
	member([]-Size, PathSizes),
	Threshold #= 30000000 - (70000000 - Size).

%% Find the smallest directory that, if deleted, would free up enough
%% space on the filesystem to run the update. What is the total size
%% of that directory?
problemB(Result) :-
	input_file(File),
	input(Commands, File),
	commands_tree(Commands, Tree),
	tree_path_sizes(Tree, Tree, PathSizes),
	maplist(path_size_size, PathSizes, Sizes0),
	free_threshold(PathSizes, Threshold),
	tfilter(size_above(Threshold), Sizes0, Sizes),
	list_min(Sizes, Result).

%% ?- problemB(Result).
%@    Result = 5649896
%@ ;  false.
