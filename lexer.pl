:- consult(helpers).

digit(X) :- string_chars("0123456789", Digits), member(X, Digits), !.
letter(X) :- 
    string_chars("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ", Letters), 
    member(X, Letters), !.

comment((Col, Line), [comment((Col, Line), End)|Tokens], Out) --> 
    ['(', '*'],
    { Col1 is Col + 2 },
    comment_content((Col1, Line), [comment_end(End)|Tokens], Out).

comment_content((Col, Line), [comment_end((Col1, Line))|Tokens], Out) --> 
    ['*', ')'], !,
    { Col1 is Col + 2 },
    lexer((Col1, Line), Tokens, Out).
comment_content((_, Line), Tokens, Out) --> 
    ['\n'], !,
    { Line1 is Line + 1 }, 
    comment_content((1, Line1), Tokens, Out).
comment_content((Col, Line), Tokens, Out) --> 
    [_],
    { Col1 is Col + 1 }, 
    comment_content((Col1, Line), Tokens, Out).


unexpected(In, TokensOut, Out) -->
    unexpected_(In, [unexpected(Xs)|Tokens], Out),
    { Xs = [] -> TokensOut = Tokens; TokensOut = [unexpected(Xs, In)|Tokens] }.

unexpected_(In, [unexpected([])|Tokens], Out) -->
    whitespace(In, Tokens, Out),
    !.

unexpected_((Col, Line), [unexpected([X|Xs])|Tokens], Out) -->
    [X],
    !,
    { Col1 is Col + 1 },
    unexpected_((Col1, Line), [unexpected(Xs)|Tokens], Out).

unexpected_(In, [unexpected([])], In) -->
    [], !.

lexer(In, Tokens, Out) --> 
    (comment(In, Tokens, Out); 
    whitespace(In, Tokens, Out);
    integer(In, Tokens, Out);
    ident(In, Tokens, Out);
    keyword(In, Tokens, Out);
    others(In, Tokens, Out);
    unexpected(In, Tokens, Out)),
    !.
lexer(Out, [], Out) --> [].

integer((Col, Line), [integer(Int, (Col, Line))|Tokens], Out) -->
    [D], { digit(D) }, !,
    { Col1 is Col + 1 },
    integer1((Col1, Line), Acc, Out1),
    { number_chars(Int, [D|Acc]) },
    lexer(Out1, Tokens, Out).
integer1((Col, Line), [D|Acc], Out) -->
    [D], { digit(D) },
    { Col1 is Col + 1 },
    integer1((Col1, Line), Acc, Out).
integer1(Out, [], Out) --> [].

ident((Col, Line), [ident(Ident, (Col, Line))|Tokens], Out) -->
    [I], { letter(I) },
    { Col1 is Col + 1 },
    ident1((Col1, Line), Acc, Out1),
    !,
    { string_chars(Ident, [I|Acc]), string_to_atom(Ident, IdentAtom), \+ keywordK(IdentAtom)  },
    lexer(Out1, Tokens, Out).
ident1((Col, Line), [I|Acc], Out) -->
    [I], { digit(I); letter(I) },
    { Col1 is Col + 1 },
    ident1((Col1, Line), Acc, Out).
ident1(Out, [], Out) --> [].

keywordK(Atom) :- member(Atom, ['if', 'then', 'else', 'fi', 'while', 'do', 'od', 'div', 'mod', 'or', 'and', 'not', 'let', 'fun', 'nuf']), !.

keyword((Col, Line), [keyword(Keyword, (Col, Line))|Tokens], Out) -->
    [K], { letter(K) },
    { Col1 is Col + 1 },
    keyword1((Col1, Line), Acc, Out1),
    { atomic_list_concat([K|Acc], Keyword), keywordK(Keyword)  },
    lexer(Out1, Tokens, Out).
keyword1((Col, Line), [K|Acc], Out) -->
    [K], { digit(K); letter(K) },
    { Col1 is Col + 1 },
    keyword1((Col1, Line), Acc, Out).
keyword1(Out, [], Out) --> [].

others((Col, Line), [char(W, (Col, Line))|Tokens], Out) -->
    [W1, W2], 
    { atomic_list_concat([W1, W2], W), member(W, ['<=', '>=', '<>', ':=']), ! },
    { Col1 is Col + 2 },
    lexer((Col1, Line), Tokens, Out).
others((Col, Line), [char(W, (Col, Line))|Tokens], Out) -->
    [W], { member(W, ['+', '-', '*', '^', '=', '<', '>', ';', '(', ')', '[', ']', '{', '}', ',']), ! },
    { Col1 is Col + 1 },
    lexer((Col1, Line), Tokens, Out).

whitespace((_, Line), Tokens, Out) -->
    (['\r', '\n']; ['\n']), !,
    { Line1 is Line + 1 },
    lexer((1, Line1), Tokens, Out).
whitespace((Col, Line), Tokens, Out) -->
    [W], { member(W, ['\t', ' ']), ! }, !,
    { Col1 is Col + 1 },
    lexer((Col1, Line), Tokens, Out).

skip_comments([], []).
skip_comments([comment(_, _)|Ts], Ps) :- skip_comments(Ts, Ps), !.
skip_comments([X|Ts], [X|Ps]) :- skip_comments(Ts, Ps).

readable_tokens([], []).
readable_tokens([comment(_, _)|Ts], Ps) :- readable_tokens(Ts, Ps), !.
readable_tokens([integer(I, _)|Ts], [integer(I)|Ps]) :- readable_tokens(Ts, Ps), !.
readable_tokens([ident(I, _)|Ts], [ident(I)|Ps]) :- readable_tokens(Ts, Ps), !.
readable_tokens([keyword(I, _)|Ts], [keyword(I)|Ps]) :- readable_tokens(Ts, Ps), !.
readable_tokens([char(C, _)|Ts], [char(C)|Ps]) :- readable_tokens(Ts, Ps), !.
readable_tokens([X|Ts], [X|Ps]) :- readable_tokens(Ts, Ps).

lex(Str, PTokens) :-
    string_chars(Str, Ls),
    !,
    phrase(lexer((1, 1), Tokens, _), Ls),
    check_unexpected_tokens(Tokens),
    skip_comments(Tokens, PTokens).

check_unexpected_tokens([unexpected(TextList, (Col, Line))|_]) :-
    string_chars(Text, TextList),
    any_list_concat(["Unexpected characters in source code starting at line ", Line, ", column ", Col, ": '", Text,"'!"], Msg),
    writeln(Msg),
    throw(unkown_tokens_in_input).

check_unexpected_tokens([_|Xs]) :-
    check_unexpected_tokens(Xs).

check_unexpected_tokens([]).

% lex_file(Path, PTokens) :-
%     read_file_to_string(Path, Str, []),
%     lex(Str, PTokens).