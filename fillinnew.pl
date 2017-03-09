%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
    % 1th：
    puzzle_with_vars(Puzzle, PuzzleWithVars),
    % 2th：
    slots_from_puzzle(PuzzleWithVars, Slots),
    % A valid puzzle for solving must have a wordlist that is equal in length
    % to the number of slots, as each word must go into a slot.
    same_length(Slots, WordList),
    % length(Slots, LenSlots),
    % length(WordList, LenWordList),
    % LenSlots == LenWordList,
    % 3th：
    fillin_all_words(Slots, WordList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1th：START

% %%%% puzzle_with_vars(Puzzle, PuzzleWithVars) %%%%
% should hold when PuzzleWithVars is identical to Puzzle except all of the
% underscore characters have been replaced with logical variables.
puzzle_with_vars([], []).
puzzle_with_vars(Rows, PuzzleWithVars) :-
    % inner function：
    maplist(row_to_vars, Rows, PuzzleWithVars).


% %%%% row_to_vars(Row, RowWithVars) %%%%
% should hold when Row is a row of an unfilled puzzle.
% RowWithVars should be the same as Row but
% should change all the underscores in the puzzle to logical variables.
row_to_vars([], []).
row_to_vars(Row, RowWithVars) :-
    maplist(replace_underscore_with_var, Row, RowWithVars).


% %%%% replace_underscore_with_var(Char, Var) %%%%
% should hold when Var is a logical variable if Char is an underscore
% and Var is equal to Char if Char is anything else. i.e. it replaces all
% underscores with logical variables and keeps all other values for Char.
replace_underscore_with_var('_', _).
replace_underscore_with_var(Char, Char) :- Char \= '_'.

% 1th: END
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2th：START

% %%%% slots_from_puzzle(Puzzle, Slots) %%%%

% should hold when Slots is a list of all the slots in both the rows and
% columns of the Puzzle.
% Puzzle is a list of lists of characters, one list per puzzle row.
% This predicate finds all horizontal and vertical slots in the puzzle.
slots_from_puzzle(Puzzle, Slots) :-
    % ----------------------------------------------------------------
    % RowSlots are defined to only have length greater than one
    slots_from_all_rows(Puzzle, RowSlots),
    % inner function:
    include(length_greater_than_one, RowSlots, PrunedRowSlots),
    % ----------------------------------------------------------------
    % inner function：rolling-over the puzzle
    transpose(Puzzle, TransposedPuzzle),
    % ----------------------------------------------------------------
    % ColumnSlots are defined to only have length greater than one
    slots_from_all_rows(TransposedPuzzle, ColumnSlots),
    include(length_greater_than_one, ColumnSlots, PrunedColumnSlots),
    % ----------------------------------------------------------------
    % add RowSlots and ColumnSlots together:
    append(PrunedRowSlots, PrunedColumnSlots, Slots).
    % ----------------------------------------------------------------

% %%%% length_greater_than_one(List) %%%%

% should hold when the length of List is greater than 1.
length_greater_than_one(List) :-
    length(List, Len),
    Len > 1.


% %%%% slots_from_all_rows(Rows, Slots) %%%%

% should hold when Slots is a list of all the slots in all of the Rows.
% Rows is a list of lists of characters, one list per row in the puzzle.
% This predicate gets all the slots in every row of the puzzle.
slots_from_all_rows([], []).
slots_from_all_rows([Row|Rows], Slots) :-
    slots_from_row(Row, RowSlots),
    % Append is used here as using each RowSlots is a list of slots
    % and using cons results in a list of RowSlots rather than a list
    % of slots which is what is desired.
    append(RowSlots, Slots1, Slots),
    slots_from_all_rows(Rows, Slots1).


% %%%% slots_from_row(Row, Slots) %%%%

% should hold when Slots contains a list of slots present in the Row.
% i.e. Slots is the result of splitting the Row on hashes.
% It calls slots_from_row/3 with an empty list as the initial value
% of the accumulator which is used to accumulate variables into slots.
% See slots_from_row/3 for more details.
slots_from_row(Row, Slots) :-
    slots_from_row(Row, [], Slots).


% %%%% slots_from_row(Row, CurrentSlot, Slots) %%%%

% is called by slots_from_row/2 to compute the slots in the row.
% It uses CurrentSlot as an accumulator, repeatedly adding characters
% into it until it encounters a hash. When it does, it adds the CurrentSlot
% into Slots and resets CurrentSlot to be an empty list.
slots_from_row([], [], []).
% If there is no hash at the end of a row to signal the end of a slot,
% CurrentSlot needs to be correctly added into Slots.
% CurrentSlot is made into a list with itself as the only element.
% This is so appending the CurrentSlot to Slots works properly.
% As Slots is a list of single slots, (e.g. [[A,#,B]])
% appending [G,#,H] results in  [[A,#,B],C,#,D] whereas appending
% [[C,#,D]] results in [[A,#,B],[C,#,D]] which is what is desired.
slots_from_row([], CurrentSlot, [CurrentSlot]) :-
    CurrentSlot \= [].
% If you encounter a hash, add the current accumulator (CurrentSlot)
% to the list of slots and reset CurrentSlot to an empty list.
% Otherwise, keep adding to the CurrentSlot which represents a single slot
% in construction.
slots_from_row([X|Xs], CurrentSlot, Slots) :-
    X == '#',
    Slots = [CurrentSlot|Slots1],
    slots_from_row(Xs, [], Slots1).

slots_from_row([X|Xs], CurrentSlot, Slots) :-
    X \== '#',
    % Append is used rather than cons so that the slot is not constructed
    % in reverse order.
    append(CurrentSlot, [X], CurrentSlot1),
    slots_from_row(Xs, CurrentSlot1, Slots).

% 2th：END
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3th：START
fillin_all_words([], []).
fillin_all_words(Slots, WordList) :-
    best_next_slot(Slots, WordList, BestSlot),
    exclude(\=(BestSlot), WordList, MatchingWords),
    member(Word, MatchingWords),
    BestSlot = Word,
    exclude(==(Word), WordList, RemainingWords),
    exclude(==(BestSlot), Slots, RemainingSlots),
    fillin_all_words(RemainingSlots, RemainingWords).


% implementation details.
best_next_slot([Slot|Slots], WordList, BestSlot) :-
    % Compute the words matching the first slot in the list of Slots
    % and treat it as the current best slot when calling best_next_slot/5.
    words_matching_slot(Slot, WordList, Count),
    best_next_slot(Slots, WordList, Count, Slot, BestSlot).

best_next_slot([], _, _, BestSlot, BestSlot).
best_next_slot([Slot|Slots], WordList, LowestMatches,
    CurrentBestSlot, BestSlot) :-
    words_matching_slot(Slot, WordList, Count),
    % If Count < LowestMatches, a better slot has been found. So update
    % the current best slot and lowest matches in the recursive call.
    (Count < LowestMatches ->
        CurrentBestSlot1 = Slot,
        LowestMatches1 = Count
    ;   CurrentBestSlot1 = CurrentBestSlot,
        LowestMatches1 = LowestMatches
    ),
    best_next_slot(Slots, WordList, LowestMatches1,
        CurrentBestSlot1, BestSlot).

words_matching_slot(Slot, WordList, Count) :-
    words_matching_slot(Slot, WordList, 0, Count).

words_matching_slot(_, [], NumMatchesAcc, NumMatchesAcc).
words_matching_slot(Slot, [Word|Words], NumMatchesAcc, Count) :-
    (Slot \= Word ->
        NumMatchesAcc1 is NumMatchesAcc
    ;   NumMatchesAcc1 is NumMatchesAcc + 1
    ), words_matching_slot(Slot, Words, NumMatchesAcc1, Count).
% 3th：END
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%% valid_puzzle(Rows) %%%%
% should hold when Rows is a list of lists, each of these having the same
% length.
% For a fillin puzzle, Rows will be a list of list of characters (one list of
% characters representing a row of the puzzle)
valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
    maplist(same_length(Row), Rows).
