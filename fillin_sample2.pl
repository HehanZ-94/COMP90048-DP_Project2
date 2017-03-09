%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% File   : fillin.pl
% Author : Maxim Lobanov - mlobanov (587697)
% Date   : 13/10/14
% Purpose: Program for Fillin Prolog Project (COMP30020 Declarative
%          Programming, Semester 2 2014). Attempts to solve a fillin
%          crossword puzzle. A fillin puzzle (or fill-it-in) is similar
%          to a crossword puzzle. The player is given a list of all the
%          words to be placed into the puzzle but not told where they
%          should go. The puzzle itself is a grid of square cells which
%          may be empty, filled with a character, or solid. An empty cell
%          is able to be filled in with a character and a solid cell may not
%          be filled in. For more information, read the project
%          specification (fillin.pdf).
%          This file's main predicate is solve_puzzle/3 which takes
%          a puzzle file and a word file and attempts to solve the puzzle.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% SWI Prolog autoloads wrong transpose/2 predicate by default, so ensure this
% library is loaded to use the correct transpose/2 predicate.
:- ensure_loaded(library(clpfd)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% main(PuzzleFile, WordlistFile, SolutionFile)
% should hold when SolutionFile is the name of the output file where
% the results of attempting to solve a fillin puzzle using the PuzzleFile
% and WordlistFile as inputs is written to.
% PuzzleFile and WordlistFile are both names of files which will be read
% in.
% PuzzleFile represents a puzzle with hashes for filled cells,
% underscores for empty cells and characters representing filled cells with
% that character. Each line of the file is a row in the puzzle.
% WordlistFile is a file containing the words which should fit in the
% puzzle. One word per line.

main(PuzzleFile, WordlistFile, SolutionFile) :-
    read_file(PuzzleFile, Puzzle),
    read_file(WordlistFile, Wordlist),
    valid_puzzle(Puzzle),
    solve_puzzle(Puzzle, Wordlist, Solved),
    print_puzzle(SolutionFile, Solved).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% I/O Predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% read_file(Filename, Content)
% should hold when Filename is the name of a file and Content is a list
% of strings (string = list of characters), one string per line of the
% file.
% It will read all of the contents (i.e. every line) of the file.

read_file(Filename, Content) :-
    open(Filename, read, Stream),
    read_lines(Stream, Content),
    close(Stream).

% read_lines(Stream, Content)
% should hold when Stream is an already opened file (e.g. if opened with
% open/3 in read mode, Stream is equivalent to third argument of open/3) and
% Content is a list of strings (string = list of characters),
% one string per line of the file that is opened to produce Stream.
% It will read all the lines of the file.

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

% read_line(Stream, Line, Last)
% should hold when Stream is an already opened file (e.g. if opened with
% open/3 in read mode, Stream is equivalent to third argument of open/3),
% Line is a single line of the file that was read in and Last is either
% true or false, being true if an EOF character was read.
% This will read a single line of the file and set Line to be the
% content of the single line (as a string). Last will indicate whether
% the line that was read is the last line of the file.

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

% print_puzzle(SolutionFile, Puzzle)
% should hold when SolutionFile is the name of a file to which output
% will be printed to and Puzzle is a list of list of characters (one
% list of chararacters per row of the Puzzle) which is being printed
% to the SolutionFile.
% This will write the entire Puzzle to the SolutionFile provided.

print_puzzle(SolutionFile, Puzzle) :-
    open(SolutionFile, write, Stream),
    maplist(print_row(Stream), Puzzle),
    close(Stream).

% print_row(Stream, Row)
% should hold when Stream is an already opened file (e.g. if opened with
% open/3 in read mode, Stream is equivalent to third argument of open/3)
% and Row is a single row of the puzzle (i.e. a list of characters).
% This will write the Row to the Stream provided.
print_row(Stream, Row) :-
    maplist(put_puzzle_char(Stream), Row),
    nl(Stream).

% put_puzzle_char(Stream, Char)
% should hold when Stream is an already opened file (e.g. if opened with
% open/3 in read mode, Stream is equivalent to third argument of open/3)
% and Char is a single character.
% This will write an underscore to the Stream if Char is a variable,
% otherwise, it will write the Char itself to the Stream.
put_puzzle_char(Stream, Char) :-
    (   var(Char)
    ->  put_char(Stream, '_')
    ;   put_char(Stream, Char)
    ).

% valid_puzzle(Rows)
% should hold when Rows is a list of lists, each of these having the same
% length.
% For a fillin puzzle, Rows will be a list of list of characters (one list of
% characters representing a row of the puzzle)
valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
    maplist(same_length(Row), Rows).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The following section contains predicates which attempt to solve the
% puzzle.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% solve_puzzle(Puzzle, WordList, PuzzleWithVars)
% should hold when PuzzleWithVars is a solved version of Puzzle, with the
% empty slots filled in with words from WordList.
% A slot is a list of variables, characters or both that represent a location
% in the puzzle where a word may be filled in. There are both horizontal
% and vertical slots in the puzzle.
% Puzzle should be a list of lists of characters (single-character atoms),
% one list per puzzle row.  WordList is also a list of lists of
% characters, one list per word.
% After calling the predicate, PuzzleWithVars should be identical to Puzzle
% except with all underscore characters replaced with their correct letters
% to form a solved puzzle.
% The algorithm used to solve the puzzle is as follows:
%   1.Replace all underscores (empty cells) in the puzzle with
%     logical variables.
%   2.Construct a list of slots, where each slot is a list
%     of logical variables representing a single square in the puzzle.
%     This ensures that if the same variable is used for the same slot
%     in either a horizontal or vertical orientation, when unifying the
%     variable, it will be correctly unified horizontally and vertically.
%   3.Repeatedly fill in words into the puzzle until it is solved.
%     This is done by selecting a slot and a word in the puzzle and
%     unifying the word with the slot and recursing. Details about how the
%     word and slot are chosen are described in fillin_all-words/2 and
%     best_next_slot/3.

solve_puzzle(Puzzle, WordList, PuzzleWithVars) :-
    fillpuzzleLV(Puzzle, PuzzleWithVars),
    getallslots(PuzzleWithVars, Slots),

    same_length(Slots, WordList),
    fillSlots(Slots, WordList).



fillpuzzleLV([], []).
fillpuzzleLV(Rows, PuzzleWithVars) :-
    maplist(sepatate_to_rows, Rows, PuzzleWithVars).



sepatate_to_rows([], []).
sepatate_to_rows(Row, RowWithVars) :-
    maplist(replace_underscore_with_var, Row, RowWithVars).



replace_underscore_with_var(Npuzz, Fpuzz) :-
    ( Npuzz == '_', Fpuzz = _ ; Npuzz \='_', Fpuzz = Npuzz).


getallslots(Puzzle, Slots) :-
    get_all_slots(Puzzle, RowSlots),

    include(length_greater_than_one, RowSlots, PrunedRowSlots),
    transpose(Puzzle, TransposedPuzzle),
    get_all_slots(TransposedPuzzle, ColumnSlots),
    include(length_greater_than_one, ColumnSlots, PrunedColumnSlots),
    append(PrunedRowSlots, PrunedColumnSlots, Slots).



length_greater_than_one(List) :-
    length(List, Len),
    Len > 1.



get_all_slots([], []).
get_all_slots([Row|Rows], Slots) :-
    get_all_slots_from_single_row(Row, RowSlots),

    append(RowSlots, Slots1, Slots),
    get_all_slots(Rows, Slots1).



get_all_slots_from_single_row(Row, Slots) :-
    get_all_slots_from_single_row(Row, [], Slots).



get_all_slots_from_single_row([], [], []).

get_all_slots_from_single_row([], CurrentSlot, [CurrentSlot]) :-
    CurrentSlot \= [].

get_all_slots_from_single_row([X|Xs], CurrentSlot, Slots) :-
    ( X == '#',
      Slots = [CurrentSlot|Slots1],
      get_all_slots_from_single_row(Xs, [], Slots1)
      ; X \== '#',
        append(CurrentSlot, [X], CurrentSlot1),
        get_all_slots_from_single_row(Xs, CurrentSlot1, Slots)
    ).


fillSlots([], _).
fillSlots([Slot|Slots], WordList) :-
    %提取出当前的slot一共有几个词可以匹配（通过canunify来判断一共slot和一个word）
	   getMatches(Slot, WordList, 0, Matches),
     %选择出当前最好的slot就是匹配词数最少的slot
	   leastMatchs(Slots, WordList, Matches, Slot, BestSlot, [], RemainingSlots),
	   fillinSlot(BestSlot, WordList, [], RemainingWords),
	   fillSlots(RemainingSlots, RemainingWords).




canMatch([],[]).
canMatch([W1|Word1],[W2|Word2]) :-
   (   (W1==W2;var(W1);var(W2))
      ->  canMatch(Word1,Word2)
   ).

getMatches(_,[], CurrentCount, CurrentCount).
getMatches(Slot, [Word|RestWords], CurrentCount, Count) :-
    ( canMatch(Slot, Word) ->
	  getMatches(Slot, RestWords, CurrentCount+1, Count)
	; getMatches(Slot, RestWords, CurrentCount, Count)
	).



leastMatchs([], _, Answer, CurrentSlot, CurrentSlot,RemainingSlotsIn, RemainingSlotsIn) :-
	Answer > 0.
leastMatchs([Slot|OtherSlots], WordList, LeastMatches, CurrentSlot,BestSlot, RemainingSlotsIn, RemainingSlotsOut) :-
	getMatches(Slot, WordList, 0, TotalMatches),
	(	TotalMatches < LeastMatches ->
	    append(RemainingSlotsIn, [CurrentSlot], RestSlotsTemp),
		leastMatchs(OtherSlots, WordList, TotalMatches, Slot, BestSlot,RestSlotsTemp, RemainingSlotsOut)
	;	append(RemainingSlotsIn, [Slot], RestSlotsTemp),
		leastMatchs(OtherSlots, WordList, LeastMatches, CurrentSlot,BestSlot, RestSlotsTemp, RemainingSlotsOut)
	).


unify(Word1,Word1).

fillinSlot(_,[],_,_RemainWordsList).
fillinSlot(BestSlot, [Word|WordList], MisMatchwords, RemainWordsList) :-
unify(BestSlot, Word),
append(WordList, MisMatchwords, RemainWordsList);
append([Word], MisMatchwords, RemainWordsListTemp),
fillinSlot(BestSlot, WordList, RemainWordsListTemp, RemainWordsList).
