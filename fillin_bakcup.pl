% You can use this code to get started with your fillin puzzle solver.
:- ensure_loaded(library(clpfd)).

main(PuzzleFile, WordlistFile, SolutionFile) :-
	read_file(PuzzleFile, Puzzle),
	read_file(WordlistFile, Wordlist),
	valid_puzzle(Puzzle),
	solve_puzzle(Puzzle, Wordlist, Solved),
	print_puzzle(SolutionFile, Solved).

read_file(Filename, Content) :-
	open(Filename, read, Stream),
	read_lines(Stream, Content),
	close(Stream).

read_lines(Stream, Content) :-
	read_line(Stream, Line, Last),
	(   Last = true
	->  (   Line = []
	    ->  Content = []
	    ;   Content = [Line]
	    )
	;  Content = [Line|Content1],
	    read_lines(Stream, Content1)
	).

read_line(Stream, Line, Last) :-
	get_char(Stream, Char),
	(   Char = end_of_file
	->  Line = [],
	    Last = true
	; Char = '\n'
	->  Line = [],
	    Last = false
	;   Line = [Char|Line1],
	    read_line(Stream, Line1, Last)
	).

print_puzzle(SolutionFile, Puzzle) :-
	open(SolutionFile, write, Stream),
	maplist(print_row(Stream), Puzzle),
	close(Stream).

print_row(Stream, Row) :-
	maplist(put_puzzle_char(Stream), Row),
	nl(Stream).

put_puzzle_char(Stream, Char) :-
	(   var(Char)
	->  put_char(Stream, '_')
	;   put_char(Stream, Char)
	).

valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
	maplist(samelength(Row), Rows).


samelength([], []).
samelength([_|L1], [_|L2]) :-
	same_length(L1, L2).


% solve_puzzle(Puzzle0, WordList, Puzzle)
% should hold when Puzzle is a solved version of Puzzle0, with the
% empty slots filled in with words from WordList.  Puzzle0 and Puzzle
% should be lists of lists of characters (single-character atoms), one
% list per puzzle row.  WordList is also a list of lists of
% characters, one list per word.
%
% This code is obviously wrong: it just gives back the unfilled puzzle
% as result.  You'll need to replace this with a working
% implementation.

% solve_puzzle(Puzzle, _, Puzzle).
solve_puzzle(Puzzle, WordList, PuzzleWithVars) :-
  % 1th：
  puzzle_with_vars(Puzzle, PuzzleWithVars),
  % 2th：
	slots_from_puzzle(PuzzleWithVars, FinalRowSlots),
  transpose(PuzzleWithVars, TransposedPuzzle),
	slots_from_puzzle(TransposedPuzzle, FinalColumnSlots),

  append(FinalRowSlots, FinalColumnSlots, Slots),
  same_length(Slots, WordList),
  % 3th：
	fillSlots(Slots, WordList).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1th：START
% %%%% puzzle_with_vars(Puzzle, PuzzleWithVars) %%%%
puzzle_with_vars([], []).
puzzle_with_vars(Rows, PuzzleWithVars) :-
	maplist(row_to_vars, Rows, PuzzleWithVars).

% %%%% row_to_vars(Row, RowWithVars) %%%%
row_to_vars([], []).
row_to_vars(Row, RowWithVars) :-
	maplist(replace_underscore_with_var, Row, RowWithVars).

% %%%% replace_underscore_with_var(Char, Var) %%%%
replace_underscore_with_var('_', _).
replace_underscore_with_var(Char, Char) :- Char \= '_'.

% 1th: END
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2th：START
slots_from_puzzle(Puzzle, FinalRowSlots) :-
	slots_from_all_rows(Puzzle, RowSlots),
	include(length_greater_than_one, RowSlots, FinalRowSlots).

% %%%% slots_from_all_rows(Rows, Slots) %%%%
slots_from_all_rows([], []).
slots_from_all_rows([Row|Rows], Slots) :-
  slots_from_row(Row, RowSlots),
  append(RowSlots, Slots1, Slots),
  slots_from_all_rows(Rows, Slots1).

% %%%% slots_from_row(Row, Slots) %%%%
slots_from_row(Row, Slots) :-
  slots_from_row(Row, [], Slots).

% %%%% slots_from_row(Row, CurrentSlot, Slots) %%%%
slots_from_row([], [], []).
slots_from_row([], CurrentSlot, [CurrentSlot]) :-
  CurrentSlot \= [].

% slots_from_row([X|Xs], CurrentSlot, Slots) :-
%   X == '#',
%   Slots = [CurrentSlot|Slots1],
%   slots_from_row(Xs, [], Slots1).
%
% slots_from_row([X|Xs], CurrentSlot, Slots) :-
%   X \== '#',
%   append(CurrentSlot, [X], CurrentSlot1),
%   slots_from_row(Xs, CurrentSlot1, Slots).

slots_from_row([X|Xs], CurrentSlot, Slots) :-
	(X \== '#'
	-> append(CurrentSlot, [X], CurrentSlot1),
		 slots_from_row(Xs, CurrentSlot1, Slots)
	; X == '#',
	   Slots = [CurrentSlot|Slots1],
		 slots_from_row(Xs, [], Slots1)
	).
% %%%% length_greater_than_one(List) %%%%
length_greater_than_one(List) :-
	length(List, Len), Len > 1.
% 2th：END
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3th:
% %%%% fillSlots(Slots, WordList) %%%%
fillSlots([], _).
fillSlots([Slot|Slots], WordList) :-
	% Workout how many combos the first word has
	getCombos(Slot, WordList, 0, Matches),
	% Calculate the slot with the least matches
	leastCombos(Slots, WordList, Matches, Slot, BestSlot, [], NewSlots),
	% Fill a slot
	fillSlot(BestSlot, WordList, [], NewWordList),
	% Fill remaining slots
	fillSlots(NewSlots, NewWordList).

% leastCombos(Slots, Wordlist, LeastMatches, CurrentSlot, BestSlot, NewSlotsIn, NewSlotsOut)
leastCombos([], _, Answer, CurrentSlot, CurrentSlot,
NewSlotsIn, NewSlotsIn) :-
	% No point trying to fill the slot if nothing can fill it
	Answer > 0.
leastCombos([Slot|OtherSlots], WordList, LeastMatches, CurrentSlot,
BestSlot, NewSlotsIn, NewSlotsOut) :-
	% Workout how many combos there are for the given slot
	getCombos(Slot, WordList, 0, TotalMatches),
	(	TotalMatches < LeastMatches
	->	% This is the new best slot, update our values
		append(NewSlotsIn, [CurrentSlot], NewSlotsTemp),
		leastCombos(OtherSlots, WordList, TotalMatches, Slot, BestSlot,
		NewSlotsTemp, NewSlotsOut)

	;	% Old Slot is better, ignore this slot
		append(NewSlotsIn, [Slot], NewSlotsTemp),
		leastCombos(OtherSlots, WordList, LeastMatches, CurrentSlot,
		BestSlot, NewSlotsTemp, NewSlotsOut)
	).

% %%%% getCombos(Slot, WordList, CurrentMatches, TotalMatches) %%%%
getCombos(_, [], CurrentMatches, CurrentMatches).
getCombos(Slot, [Word|OtherWords], CurrentMatches, TotalMatches) :-
	(	canUnify(Slot, Word)
	->	getCombos(Slot, OtherWords, CurrentMatches+1, TotalMatches)
	;	getCombos(Slot, OtherWords, CurrentMatches, TotalMatches)
	).

% %%%% canUnify(Word1, Word2) %%%%
canUnify([], []).
canUnify([W1|Word1], [W2|Word2]) :-
	(	(W1 == W2; var(W1); var(W2))
	->	canUnify(Word1, Word2)
	).

% %%%% unify(Word1, Word2) %%%%
unify(Word1, Word1).

% %%%% fillSlot(Slot, WordList, NewWordListIn, NewWordListOut) %%%%
fillSlot(Slot, [Word|WordList], NewWordListIn, NewWordListOut) :-
	unify(Slot, Word),
	append(WordList, NewWordListIn, NewWordListOut);
	append(NewWordListIn, [Word], NewWordListTemp),
	fillSlot(Slot, WordList, NewWordListTemp, NewWordListOut).
