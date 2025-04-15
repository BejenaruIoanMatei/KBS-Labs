:- use_module(library(clpfd)).

solve_crypto(WordsList, Solution) :-
    append(AddendsList, [Result], WordsList),
    
    collect_letters(WordsList, Letters),
    
    length(Letters, N),
    length(Values, N),
    Values ins 0..9,
    
    pairs_keys_values(Solution, Letters, Values),
    
    constrain_first_letters(WordsList, Solution),
    
    all_distinct(Values),
    
    maplist(word_to_number(Solution), AddendsList, Addends),
    word_to_number(Solution, Result, ResultNumber),
    
    sum(Addends, #=, ResultNumber),
    
    labeling([ff], Values).

collect_letters(Words, UniqueLetters) :-
    maplist(atom_chars, Words, CharsList),
    append(CharsList, AllChars),
    list_to_set(AllChars, UniqueLetters).

constrain_first_letters(Words, Solution) :-
    maplist(constrain_first_letter(Solution), Words).

constrain_first_letter(Solution, Word) :-
    atom_chars(Word, [FirstChar|_]),
    member(FirstChar-Value, Solution),
    Value #\= 0.

word_to_number(Solution, Word, Number) :-
    atom_chars(Word, Chars),
    maplist(char_to_digit(Solution), Chars, Digits),
    foldl(add_digit, Digits, 0, Number).

char_to_digit(Solution, Char, Digit) :-
    member(Char-Digit, Solution).

add_digit(Digit, Acc, Result) :-
    Result #= Acc * 10 + Digit.

print_solution(Solution) :-
    foreach(member(Letter-Value, Solution),
            format('~w = ~w~n', [Letter, Value])).

show_numeric_solution(WordsList, Solution) :-
    append(AddendsList, [Result], WordsList),
    maplist(word_to_numeric_display(Solution), AddendsList, NumericAddends),
    word_to_numeric_display(Solution, Result, NumericResult),
    format('Equation: ', []),
    print_list_with_plus(NumericAddends),
    format(' = ~w~n', [NumericResult]).

word_to_numeric_display(Solution, Word, NumericDisplay) :-
    atom_chars(Word, Chars),
    maplist(char_to_digit(Solution), Chars, Digits),
    atomic_list_concat(Digits, '', NumericAtom),
    atom_number(NumericAtom, NumericDisplay).

print_list_with_plus([]) :- !.
print_list_with_plus([X]) :- !, format('~w', [X]).
print_list_with_plus([X|Xs]) :- format('~w + ', [X]), print_list_with_plus(Xs).

crypto_solve :-
    write('Enter cryptarithmetic puzzle as a list (e.g., [send,more,money]): '),
    read(WordsList),
    (solve_crypto(WordsList, Solution) ->
        nl, write('Solution found:'), nl,
        print_solution(Solution),
        nl, show_numeric_solution(WordsList, Solution)
    ;
        nl, write('No solution found.')
    ).
    
:- initialization(crypto_solve).