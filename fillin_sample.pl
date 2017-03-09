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
    maplist(same_length(Row), Rows).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
solve_puzzle(Puzzle, WordList, PuzzleWithVars) :-
    % 1th:
    puzzle_with_vars(Puzzle, PuzzleWithVars),
    % 2th:
    slots_from_puzzle(PuzzleWithVars, Slots),
	same_length(Slots, WordList),
    % 3th:
    fillSlots(Slots, WordList).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1th:
puzzle_with_vars([], []).
puzzle_with_vars(Rows, PuzzleWithVars) :-
    maplist(row_to_vars, Rows, PuzzleWithVars).

row_to_vars([], []).
row_to_vars(Row, RowWithVars) :-
    maplist(replace_underscore_with_var, Row, RowWithVars).


replace_underscore_with_var('_', _).
replace_underscore_with_var(Char, Char) :- Char \= '_'.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2th:
slots_from_puzzle(Puzzle, Slots) :-
    get_slot(Puzzle, RowSlots),
    transpose(Puzzle, TransposedPuzzle),
    get_slot(TransposedPuzzle, ColumnSlots),
    append(RowSlots,ColumnSlots,TotalSlots),
	exclude(length_less_than_two, TotalSlots,Slots).

length_less_than_two(List) :-
    length(List,Len),
    Len < 2.

get_slot([], []).
get_slot([Row|Rows], Slots) :-
    slots_from_row(Row, RowSlots),
    append(RowSlots, Slots1, Slots),
    get_slot(Rows, Slots1).

slots_from_row(Row, Slots) :-
    slots_from_row(Row, [], Slots).

slots_from_row([], [], []).
slots_from_row([], CurrentSlot, [CurrentSlot]) :-
    CurrentSlot \= [].

slots_from_row([Char|Tail], CurrentSlot, Slots) :-
    ( Char == '#' ->
    (Slots = [CurrentSlot|Slots1],
    slots_from_row(Tail, [], Slots1)
	)
	;
    (append(CurrentSlot, [Char], CurrentSlot1),
    slots_from_row(Tail, CurrentSlot1, Slots))
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3th:
fillSlots([], _).
fillSlots([Slot|Slots], WordList) :-
	getCombos(Slot, WordList, 0, Matches),
	leastCombos(Slots, WordList, Matches, Slot, BestSlot, [], RemainingSlots),
	fillSlot(BestSlot, WordList, [], RemainingWords),
	fillSlots(RemainingSlots, RemainingWords).

leastCombos([], _, Answer, CurrentSlot, CurrentSlot,
	NewSlotsIn, NewSlotsIn) :-
	Answer > 0.
leastCombos([Slot|OtherSlots], WordList, LeastMatches, CurrentSlot,
	BestSlot, NewSlotsIn, NewSlotsOut) :-
	getCombos(Slot, WordList, 0, TotalMatches),
	(	TotalMatches < LeastMatches
	->
	    append(NewSlotsIn, [CurrentSlot], NewSlotsTemp),
		leastCombos(OtherSlots, WordList, TotalMatches, Slot, BestSlot,
			NewSlotsTemp, NewSlotsOut)
	;
		append(NewSlotsIn, [Slot], NewSlotsTemp),
		leastCombos(OtherSlots, WordList, LeastMatches, CurrentSlot,
			BestSlot, NewSlotsTemp, NewSlotsOut)
	).

getCombos(_, [], CurrentMatches, CurrentMatches).
getCombos(Slot, [Word|OtherWords], CurrentMatches, TotalMatches) :-
	(	canUnify(Slot, Word)
	->	getCombos(Slot, OtherWords, CurrentMatches+1, TotalMatches)
	;	getCombos(Slot, OtherWords, CurrentMatches, TotalMatches)
	).

canUnify([], []).
canUnify([W1|Word1], [W2|Word2]) :-
	(	(W1 == W2; var(W1); var(W2))
	->	canUnify(Word1, Word2)
	).

unify(Word1,Word1).

fillSlot(Slot, [Word|WordList], NewWordListIn, NewWordListOut) :-
	unify(Slot, Word),
	append(WordList, NewWordListIn, NewWordListOut);
	append(NewWordListIn, [Word], NewWordListTemp),
	fillSlot(Slot, WordList, NewWordListTemp, NewWordListOut).
