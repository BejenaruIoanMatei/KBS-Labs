:- use_module(library(clpfd)).
% Constraint Logic Programming library

solve_crypto(WordsList, Solution) :-
   
    %   Pentru exemplul [send, more, money]:
    %           - AddendsList = [send, more]
    %           - Result = money
    %       Letters = [s,e,n,d,m,o,r,y]
    %       Values = [Vs,Ve,Vn,Vd,Vm,Vo,Vr,Vy]
    %       Solution = [s-Vs, e-Ve, n-Vd, d-Vd, m-Vm, o-Vo, r-Vr, y-Vy]
    %       Constrangeri adaugate: 
    %               - variabilele intre 0-9
    %               - prima litera din cuvant nu poate fi 0
    %       "send" -> Vs*1000+Ve*100+Vn*10+Vd
    %       Verif ecuatia: (Vs*1000+Ve*100+Vn*10+Vd) + (Vm*1000+Vo*100+Vr*10+Ve) = (Vm*10000+Vo*1000+Vn*100+Ve*10+Vy)
    %       Asignare valida :
    %           Solution found:
                    % s = 9
                    % e = 5
                    % n = 6
                    % d = 7
                    % m = 1
                    % o = 0
                    % r = 8
                    % y = 2, verifica suma: 9567+1085 = 10652

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
    
    labeling([ff], Values). % [ff] -> euristica: first-fail, ajuta la gasirea solutiei mai repede

collect_letters(Words, UniqueLetters) :-
    % Toate literele distincte din toate cuvintele
    % De ex pentru [send, more, money]:
    %       - [s, e, n, d, m, o, r, y]
    maplist(atom_chars, Words, CharsList),
    append(CharsList, AllChars),
    list_to_set(AllChars, UniqueLetters).



constrain_first_letters(Words, Solution) :-
    % Numerele nu incep cu 0
    maplist(constrain_first_letter(Solution), Words).



constrain_first_letter(Solution, Word) :-
    % Extrage primul caracter si il constrange sa nu fie 0
    atom_chars(Word, [FirstChar|_]),
    member(FirstChar-Value, Solution),
    Value #\= 0.

word_to_number(Solution, Word, Number) :-
    % [send] -> [s, e, n, d] -> [1, 2, 3, 4] -> 1234
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
    format('Sum: ', []),
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
    write('Short example of correct input: [word1,word2,word3,result]. (dont forget the ".") \n'),
    write('Enter cryptarithmetic puzzle as a list: '),
    read(WordsList),
    (solve_crypto(WordsList, Solution) ->
        nl, write('Solution found:'), nl,
        print_solution(Solution),
        nl, show_numeric_solution(WordsList, Solution)
    ;
        nl, write('Failed boss')
    ).
    
:- initialization(crypto_solve).