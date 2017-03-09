%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% File   : fillin.pl
% Author : Yuchen Qiao (748663)
% Date   : 23/10/2016
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
solve_puzzle(Puzzle0, WordList, Puzzle) :-
  % 1th：replace all of the underscores to logical variables
  get_modified_puzzle(Puzzle0, Puzzle),
  % 2th：get all slots from rows
	get_all_slots(Puzzle, FinalRowSlots),
	% tranpose the puzzle
  transpose(Puzzle, TransposedPuzzle),
	% get all slots from columns
	get_all_slots(TransposedPuzzle, FinalColumnSlots),
	% get the total slots
  append(FinalRowSlots, FinalColumnSlots, AllSlots),
  samelength(AllSlots, WordList),
  % 3th：fill every slots with proper words from wordlist
	fill_all_slots(AllSlots, WordList).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1th：START
% %%%% get_modified_puzzle(Puzzle0, Puzzle) %%%%
% Puzzle0: the original puzzle from file
% Puzzle: the modified puzzle which change each '_' to _
get_modified_puzzle([], []).
get_modified_puzzle(Rows, PuzzleModified) :-
	maplist(get_modified_row, Rows, PuzzleModified).

% %%%% get_modified_row(Row, RowModified) %%%%
% Row: a row of an original puzzle
% RowModified: same as Row but should change all '_' to _
get_modified_row([], []).
get_modified_row(Row, RowModified) :-
	maplist(underscore_modified, Row, RowModified).

% %%%% underscore_modified(Char, Var) %%%%
% change '_' to _
% if encounter '#', dont change it
underscore_modified('_', _).
underscore_modified(Char, Char) :- Char \= '_'.

% 1th: END
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2th：START
% %%%% get_all_slots(Puzzle, FinalRowSlots) %%%%
% Puzzle: the modified puzzle as input
% FinalRowSlots: all slots from the horizontal or vertical as output
get_all_slots(Puzzle, FinalRowSlots) :-
	get_slots_rows(Puzzle, RowSlots),
	include(length_checking, RowSlots, FinalRowSlots).

% %%%% get_slots_rows(Rows, Slots) %%%%
% Rows: a list of alist of chars
% Slots: the output slots gathered from all rows of puzzle
get_slots_rows([], []).
get_slots_rows([Row|Rows], Slots) :-
	% get the slots from a single row
	get_slots_row(Row, [], RowSlots),
	% Slots1 play a role as a template warehouse
  append(RowSlots, Slots1, Slots),
  get_slots_rows(Rows, Slots1).

% %%%% get_slots_row(Row, CurrentSlot, Slots) %%%%
% Row: the row as an input
% CurrentSlot: an accumulator template store the chars
% Slots: the avaliable slots from the row

% CurrentSlot is used as an accumulator, template preserve chars into it till when
% the char is '#', then CurrentSlot will be added into Slots
% and reset the CurrentSlot
get_slots_row([], [], []).
% if there is no '#' at the end of the row, add CurrentSlot to list
get_slots_row([], CurrentSlot, [CurrentSlot]) :-
  CurrentSlot \= [].
% adding chars, which is a slot, to the CurrentSlot
% when meet '#', the CurrentSlot counting should be stoped
get_slots_row([X|Xs], CurrentSlot, Slots) :-
	(X \== '#'
	-> append(CurrentSlot, [X], CurrentSlot1),
		 get_slots_row(Xs, CurrentSlot1, Slots)
	;  X == '#',
	   Slots = [CurrentSlot|Slots1],
		 get_slots_row(Xs, [], Slots1)
	).

% %%%% length_checking(List) %%%%
% true if the length of List is greater than 1
length_checking(List) :-
	length(List, Length), Length > 1.
% 2th：END
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3th:
% %%%% fill_all_slots(Slots, WordList) %%%%
% Slots: a list of slots
% WordList: the list of words
fill_all_slots([], _).
fill_all_slots([Slot|Slots], WordList) :-
	% get the number of combos which first word can match
	getMatches(Slot, WordList, 0, Matches),
	% compute the slot with the least matches
	get_least_match(Slots, WordList, Matches, Slot, ChoosenSlot, [], NextSlots),
	% fill in the slot with the chosen word
	fill_slot(ChoosenSlot, WordList, [], NewWordList),
	fill_all_slots(NextSlots, NewWordList).

% %%%% getMatches(Slot, WordList, CurrentMatches, TotalMatches) %%%%
% Returns the number of combos for the chosen slot, with using the wordlist

% Slot:	the avaliable slot
% WordList:	the given wordlist
% CurrentMatches:	how many matches have been found
% TotalMatches:	the total number of matches
getMatches(_, [], CurrentMatches, CurrentMatches).
getMatches(Slot, [Word|RestWords], CurrentMatches, TotalMatches) :-
	(		get_is_unification(Slot, Word)
	->	getMatches(Slot, RestWords, CurrentMatches + 1, TotalMatches)
	;		getMatches(Slot, RestWords, CurrentMatches, TotalMatches)
	).

% get_least_match(Slots, Wordlist, LeastMatches, CurrentSlot, ChoosenSlot, NewSlotsIn, NewSlotsOut)
% Finds the slot with the least combo,
% which is thelowest number of words that can go into the slot

% Slots:	the avaliable slot
% Wordlist: the given wordlist
% LeastMatches:	the lowest number of combos till now
% CurrentSlot: the slot with the lowest number of combos till now
% BestSlot:	the slot with the lowest number of combos
% NewSlotsIn: the slots without CurrentSlot
% NewSlotsOut	the slots without the BestSlot
get_least_match([], _, Answer, CurrentSlot, CurrentSlot,
NewSlotsIn, NewSlotsIn) :-
	Answer > 0.

get_least_match([Slot|OtherSlots], WordList, LeastMatches, CurrentSlot,
ChoosenSlot, NewSlotsIn, NewSlotsOut) :-
	% get the number of combos for the given slot
	getMatches(Slot, WordList, 0, TotalMatches),
	(	TotalMatches < LeastMatches
		 % if find a new best slot
	-> append(NewSlotsIn, [CurrentSlot], NewSlotsTemp),
		 get_least_match(OtherSlots, WordList, TotalMatches, Slot, ChoosenSlot,
		 NewSlotsTemp, NewSlotsOut)
		 % if the old Slot is better
	;	 append(NewSlotsIn, [Slot], NewSlotsTemp),
		 get_least_match(OtherSlots, WordList, LeastMatches, CurrentSlot,
		 ChoosenSlot, NewSlotsTemp, NewSlotsOut)
	).

% %%%% fill_slot(Slot, WordList, InputList, OutputList) %%%%
% Slot:	the slot need to be filled
% WordList:	the given wordlist
% InputList: the words which are already palced in the slot
% OutputList:	the remian wordlist
fill_slot(Slot, [Word|WordList], InputList, OutputList) :-
	unification(Slot, Word),
	append(WordList, InputList, OutputList);
	% if fail, try another word
	append(InputList, [Word], TempList),
	fill_slot(Slot, WordList, TempList, OutputList).

% %%%% get_is_unification(Words1, Words2) %%%%
% Checks whether two lists of words can be unified together
get_is_unification([], []).
get_is_unification([Word1|Words1], [Word2|Words2]) :-
	(	(Word1 == Word2; var(Word1); var(Word2))
	->	get_is_unification(Words1, Words2)
	).

% %%%% unification(Words1, Words2) %%%%
% unify two lists
unification(Words1, Words1).
