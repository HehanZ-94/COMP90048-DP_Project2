:- ensure_loaded(library(clpfd)).

% The main entrypoint into the program
% PuzzleFile	The puzzle file to read in
% WordlistFile	The list of words to fill the puzzle with
% SolutionFile	The file to output the solution to
main(PuzzleFile, WordlistFile, SolutionFile) :-
	% Read in the puzzle and wordlist
	% Note: an underscore will be converted into a variable
	read_file(PuzzleFile, Puzzle),
	read_file(WordlistFile, Wordlist),

	% Ensure the puzzle is actually valid
	valid_puzzle(Puzzle),

	% Actually solve the puzzle
	solve_puzzle(Puzzle, Wordlist),

	% Output the solved puzzle to the specified file
	print_puzzle(SolutionFile, Puzzle).

% Reads the given file
% Filename	The name of the file to read
% Content	Where the content will be stored to
read_file(Filename, Content) :-
	open(Filename, read, Stream),
	read_lines(Stream, Content),
	close(Stream).

% Reads all the lines of the given stream
% Stream	The stream to read from
% Content	Where to store the content onto
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

% Reads a single line of the given steam
% Stream	The stream to read from
% Line		The line to store to
% Last		Whether this is the last line or not
read_line(Stream, Line, Last) :-
	get_char(Stream, Char),
	(   Char = end_of_file
	->  Line = [],
	    Last = true
	; Char = '\n'
	->  Line = [],
	    Last = false
	; Char = '_'
	->	Line = [NewVariable|Line1],
	    read_line(Stream, Line1, Last)
	; Line = [Char|Line1],
	    read_line(Stream, Line1, Last)
	).

% Outputs a puzzle to the given file
% SolutionFile	Name of the file to print to
% Puzzle		The puzzle to actually print
print_puzzle(SolutionFile, Puzzle) :-
	open(SolutionFile, write, Stream),
	maplist(print_row(Stream), Puzzle),
	close(Stream).

% Outputs the given row to the given stream
% Stream	The stream to print to
% Row		The row to print
print_row(Stream, Row) :-
	maplist(put_puzzle_char(Stream), Row),
	nl(Stream).

% Outputs a the given character to the given stream
% Stream	The stream to write to
% Char		The character to write
put_puzzle_char(Stream, Char) :-
	(   var(Char)
	->  put_char(Stream, '_')
	;   put_char(Stream, Char)
	).

% Checks if the given puzzle is valid
% valid_puzzle(Puzzle)
% Puzzle	The puzzle to check
valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
	maplist(samelength(Row), Rows).

% Checks of the dimensions match in  the given puzzle
% samelength(List1, List2)
% List1	The list to compare against List2
% List2	The list to compare against List1
samelength([], []).
samelength([_|L1], [_|L2]) :-
	same_length(L1, L2).


% %%%% solve_puzzle(Puzzle, WordList) %%%%
% will be true when the given puzzle can be filled in
% using the given wordlist, as a side effect, the puzzle
% will actually be filled in, if it's not already

% It works by building a list of slots that need to be filled
% Then it tries to fill those slots with the given words
%  by unifying them together

% A list of slots is really a list of lists
% The inner most lists will contain either variables that need
%  to be unified, or constants (already been unified)

solve_puzzle(Puzzle, WordList) :-
	% Grab a transposed version of the puzzle
	% inner function：
	transpose(Puzzle, PuzzleTransposed),
	% 1th:
	% Build the list of slots
	buildSlots(Puzzle, [], Slots1),
	buildSlots(PuzzleTransposed, Slots1, Slots),
	% 2th:
	% Put words into slots
	fillSlots(Slots, WordList).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1th:
% Converts a puzzle into a list of slots that can be filled in
% %%%% buildSlots(Puzzle, InputSlots, OutputSlots) %%%%
% Puzzle		The puzzle we are building slots from
% InputSlots	A list of slots to append to
% OutputSlots	A list of slots in Puzzle appended to InputSlots
buildSlots([], InputSlots, InputSlots).
buildSlots([Row|OtherRows], InputSlots, OutputSlots) :-
	processRow(Row, [], InputSlots, TempSlots),
	buildSlots(OtherRows, TempSlots, OutputSlots).

% Converts a \row of the puzzle into a list of slots that can be filled in
% %%%% processRow(PuzzleRow, CurrentWord, InputSlots, OutputSlots) %%%%
% PuzzleRow:		A row in the puzzle
% CurrentWord:	The current part of a word that is being built
% InputSlots:	A list of slots
% OutputSlots:	A list of slots, combined with all the slots in this row
processRow([], CurrentWord, InputSlots, OutputSlots) :-
	(	CurrentWord == []
	->	unify(OutputSlots, InputSlots)
	;	(	length(CurrentWord, Currentlength), 	Currentlength > 1
		->	append(InputSlots, [CurrentWord], OutputSlots)
		;	unify(OutputSlots, InputSlots)
		)
	).

processRow([Char|Rest], CurrentWord, InputSlots, OutputSlots) :-
	(	Char == '#'
	->	(	CurrentWord == []
		->	processRow(Rest, [], InputSlots, OutputSlots)
		;	(	length(CurrentWord, Currentlength), Currentlength > 1
			->	append(InputSlots, [CurrentWord], TempSlots),
				processRow(Rest, [], TempSlots, OutputSlots)
			;	processRow(Rest, [], InputSlots, OutputSlots)
			)
		)
	;	append(CurrentWord, [Char], NewCurrentWord),
		processRow(Rest, NewCurrentWord, InputSlots, OutputSlots)
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2th:
% Fills in all the slots
% It finds the slot with the least number of combos
% It then tries to fill the slot with a word
% %%%% fillSlots(Slots, WordList) %%%%
% Slots：		A list of slots that need to be filled
% WordList：	The list of words that should be placed into the slots
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
% Finds the slot with the least combinations (lowest number of
%	words that can go into it)
% leastCombos(Slots, Wordlist, LeastMatches, CurrentSlot,
%	%%%% BestSlot, NewSlotsIn, NewSlotsOut) %%%%
% Slots			The list of slots that we are searching for low combos in
% Wordlist		The list of words that need to be placed into slots
% LeastMatches	The least number of combos so far
% CurrentSlot	The slot with the least number of combos so far
% BestSlot		The slot with the least number of combos
% NewSlotsIn	The slot list, without the CurrentSlot in it
% NewSlotsOut	The slot list, without the BestSlot in it
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

% Returns the number of combinations for the given slot,
%  using the given word list
% %%%% getCombos(Slot, WordList, CurrentMatches, TotalMatches) %%%%
% Slot				The slot we are trying to find combos for
% WordList			The word list to try and match
% CurrentMatches	How many matches have been found so far
% TotalMatches		The total number of matches
getCombos(_, [], CurrentMatches, CurrentMatches).
getCombos(Slot, [Word|OtherWords], CurrentMatches, TotalMatches) :-
	(	canUnify(Slot, Word)
	->	getCombos(Slot, OtherWords, CurrentMatches+1, TotalMatches)
	;	getCombos(Slot, OtherWords, CurrentMatches, TotalMatches)
	).

% Checks if two lists can be unified
% %%%% canUnify(Word1, Word2) %%%%
% Word1	A word to try and unify with Word2
% Word2 A word to try and unify with Word1
canUnify([], []).
canUnify([W1|Word1], [W2|Word2]) :-
	(	(W1 == W2; var(W1); var(W2))
	->	canUnify(Word1, Word2)
	).

% Actually unifies two lists
% %%%% unify(Word1, Word2) %%%%
% Word1:	A word to unify with Word2
% Word2:	A word to unify with Word1
unify(Word1, Word1).

% Attempts to fill in the given slot
% %%%% fillSlot(Slot, WordList, NewWordListIn, NewWordListOut) %%%%
% Slot:				The slot we have to fill
% WordList:			The list of words to try to put into the given slot
% NewWordListIn:		List of words we have already tried in the slot
% NewWordListOut:	The word list, with the word that filled the slot removed
fillSlot(Slot, [Word|WordList], NewWordListIn, NewWordListOut) :-
	% Attempt to fill the slot
	unify(Slot, Word),
	append(WordList, NewWordListIn, NewWordListOut);

	% Failure, try next item
	append(NewWordListIn, [Word], NewWordListTemp),
	fillSlot(Slot, WordList, NewWordListTemp, NewWordListOut).
