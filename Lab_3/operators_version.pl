:- use_module(library(clpfd)).

% Main predicate to solve cryptarithmetic puzzles with various operators
solve_crypto(WordsList, Operators, Solution) :-
    % Extract the last word (result of the equation)
    append(TermsList, [Result], WordsList),
    
    % Collect all unique letters
    collect_letters(WordsList, Letters),
    
    % Each letter is assigned a digit from 0 to 9
    length(Letters, N),
    length(Values, N),
    Values ins 0..9,
    
    % Create a map of letters to values
    pairs_keys_values(Solution, Letters, Values),
    
    % First letter of each word can't be 0
    constrain_first_letters(WordsList, Solution),
    
    % All letters must have different values
    all_distinct(Values),
    
    % Convert words to numbers
    maplist(word_to_number(Solution), TermsList, Terms),
    word_to_number(Solution, Result, ResultNumber),
    
    % Apply operators between terms to get the result
    apply_operators(Terms, Operators, ResultNumber),
    
    % Find a valid solution
    labeling([ff], Values).

% Default solver that assumes all operations are addition
solve_crypto(WordsList, Solution) :-
    % Create a list of '+' operators for all terms except the last
    length(WordsList, TotalTerms),
    OperatorCount is TotalTerms - 2,  % We need one less operator than terms
    length(Operators, OperatorCount),
    maplist(=(+), Operators),
    
    % Call the main solver with addition operators
    solve_crypto(WordsList, Operators, Solution).

% Apply operators between terms to calculate the result
apply_operators([Term], [], Term) :- !.  % Base case with just one term
apply_operators([Term1, Term2 | Rest], [Op | Ops], Result) :-
    apply_operator(Term1, Op, Term2, IntermediateResult),
    apply_operators([IntermediateResult | Rest], Ops, Result).

% Apply a single operator between two terms
apply_operator(A, +, B, Result) :- Result #= A + B.
apply_operator(A, -, B, Result) :- Result #= A - B.
apply_operator(A, *, B, Result) :- Result #= A * B.
apply_operator(A, /, B, Result) :- 
    B #\= 0,  % Prevent division by zero
    A #= B * Result,  % Use multiplication to represent division to ensure integer results
    A mod B #= 0.  % Ensure division results in an integer

% Collect all unique letters from a list of words
collect_letters(Words, UniqueLetters) :-
    maplist(atom_chars, Words, CharsList),
    append(CharsList, AllChars),
    list_to_set(AllChars, UniqueLetters).

% Ensure the first letter of each word is not assigned to 0
constrain_first_letters(Words, Solution) :-
    maplist(constrain_first_letter(Solution), Words).

constrain_first_letter(Solution, Word) :-
    atom_chars(Word, [FirstChar|_]),
    member(FirstChar-Value, Solution),
    Value #\= 0.

% Convert a word to its numerical value based on the letter-digit mapping
word_to_number(Solution, Word, Number) :-
    atom_chars(Word, Chars),
    maplist(char_to_digit(Solution), Chars, Digits),
    foldl(add_digit, Digits, 0, Number).

% Find the digit value for a character
char_to_digit(Solution, Char, Digit) :-
    member(Char-Digit, Solution).

% Calculate number by adding digits with the correct place value
add_digit(Digit, Acc, Result) :-
    Result #= Acc * 10 + Digit.

% Helper predicate to pretty-print the solution
print_solution(Solution) :-
    foreach(member(Letter-Value, Solution),
            format('~w = ~w~n', [Letter, Value])).

% Show the numeric solution with operators
show_numeric_solution(WordsList, Operators, Solution) :-
    append(TermsList, [Result], WordsList),
    maplist(word_to_numeric_display(Solution), TermsList, NumericTerms),
    word_to_numeric_display(Solution, Result, NumericResult),
    format('Equation: ', []),
    print_terms_with_operators(NumericTerms, Operators),
    format(' = ~w~n', [NumericResult]).

% Default display for addition only
show_numeric_solution(WordsList, Solution) :-
    length(WordsList, TotalTerms),
    OperatorCount is TotalTerms - 2,
    length(Operators, OperatorCount),
    maplist(=(+), Operators),
    show_numeric_solution(WordsList, Operators, Solution).

word_to_numeric_display(Solution, Word, NumericDisplay) :-
    atom_chars(Word, Chars),
    maplist(char_to_digit(Solution), Chars, Digits),
    atomic_list_concat(Digits, '', NumericAtom),
    atom_number(NumericAtom, NumericDisplay).

% Print terms with operators between them
print_terms_with_operators([X], [], _) :- !, format('~w', [X]).
print_terms_with_operators([X, Y | Rest], [Op | Ops], _) :-
    format('~w ', [X]),
    write_operator(Op),
    format(' ', []),
    print_terms_with_operators([Y | Rest], Ops, _).

% Write operator symbol
write_operator(+) :- write('+').
write_operator(-) :- write('-').
write_operator(*) :- write('*').
write_operator(/) :- write('/').

% Main entry point for the program
crypto_solve :-
    write('Enter cryptarithmetic puzzle as a list (e.g., [send,more,money]): '),
    read(WordsList),
    write('Enter operators as a list (e.g., [+,+] or [+,-,*]). For all addition, just press enter: '),
    read_line_to_string(user_input, OpInput),
    (OpInput = "" ->
        % Default to all addition operators
        (solve_crypto(WordsList, Solution) ->
            nl, write('Solution found (using addition):'), nl,
            print_solution(Solution),
            nl, show_numeric_solution(WordsList, Solution)
        ;
            nl, write('No solution found.')
        )
    ;
        % Parse the operators input
        term_string(Operators, OpInput),
        (solve_crypto(WordsList, Operators, Solution) ->
            nl, write('Solution found:'), nl,
            print_solution(Solution),
            nl, show_numeric_solution(WordsList, Operators, Solution)
        ;
            nl, write('No solution found.')
        )
    ).

% Alternative entry with specified operators
crypto_solve_with_ops :-
    write('Enter cryptarithmetic puzzle as a list (e.g., [send,more,money]): '),
    read(WordsList),
    write('Enter operators list (e.g., [+,-,*]): '),
    read(Operators),
    (solve_crypto(WordsList, Operators, Solution) ->
        nl, write('Solution found:'), nl,
        print_solution(Solution),
        nl, show_numeric_solution(WordsList, Operators, Solution)
    ;
        nl, write('No solution found.')
    ).

:- initialization(crypto_solve).