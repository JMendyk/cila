% ---------------------------------- Lexer -------------------------------------

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

lexer(In, Tokens, Out) --> 
    (comment(In, Tokens, Out); 
    whitespace(In, Tokens, Out);
    integer(In, Tokens, Out);
    ident(In, Tokens, Out);
    keyword(In, Tokens, Out);
    others(In, Tokens, Out)),
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

keywordK(Atom) :- member(Atom, ['if', 'then', 'else', 'fi', 'while', 'do', 'od', 'div', 'mod', 'or', 'and', 'not']), !.

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

others((Col, Line), [W|Tokens], Out) -->
    [W1, W2], 
    { atomic_list_concat([W1, W2], W), member(W, ['<=', '>=', '<>', ':=']), ! },
    { Col1 is Col + 2 },
    lexer((Col1, Line), Tokens, Out).
others((Col, Line), [W|Tokens], Out) -->
    [W], { member(W, ['+', '-', '*', '^', '=', '<', '>', ';', '(', ')', '[', ']']), ! },
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

pretty_tokens([], []).
pretty_tokens([comment(_, _)|Ts], Ps) :- pretty_tokens(Ts, Ps), !.
pretty_tokens([integer(I, _)|Ts], [integer(I)|Ps]) :- pretty_tokens(Ts, Ps), !.
pretty_tokens([ident(I, _)|Ts], [ident(I)|Ps]) :- pretty_tokens(Ts, Ps), !.
pretty_tokens([keyword(I, _)|Ts], [keyword(I)|Ps]) :- pretty_tokens(Ts, Ps), !.
pretty_tokens([X|Ts], [X|Ps]) :- pretty_tokens(Ts, Ps).

lex(Str, PTokens) :-
    string_chars(Str, Ls),
    !,
    phrase(lexer((1, 1), Tokens, _), Ls),
    pretty_tokens(Tokens, PTokens).

lex_file(Path, PTokens) :-
    read_file_to_string(Path, Str, []),
    lex(Str, PTokens).

% ---------------------------------- Parser ------------------------------------

term_expansion(lrec(Pred, Sngl, Sep, Combine), [
    Base --> (Single1, One), 
    One --> ([Sep], !, Single2, OneComb),
    Ground --> []
]) :-
    Base =.. [Pred, Ast],
    Single1 =.. [Sngl, Ast1],
    atom_concat(Pred, 1, Pred1),
    One =.. [Pred1, Ast1, Ast],
    Single2 =.. [Sngl, Ast2],
    Combine =.. UnpackedCombine,
    append(UnpackedCombine, [Ast1, Ast2], UnpackedCombExpr),
    CombExpr =.. UnpackedCombExpr,
    OneComb =.. [Pred1, CombExpr, Ast],
    Ground =.. [Pred1, Ast, Ast].

parser(Ast) --> program(Ast).

program(Ast) --> instruction(Ast1), program1([Ast1|X]-X, Ast).
program1(H-T, Ast) --> instruction(Ast2), !, { T = [Ast2|T1] }, program1(H-T1, Ast).
program1(H-T, Ast) --> [], { T = [], Ast = H }.

instruction(ident(I, Content) := Ast) --> [ident(I)], ['['], arith_expr(Content), [']'], [':='], !, arith_expr(Ast), [';'].
instruction(ident(I) := Ast) --> [ident(I), ':='], !, arith_expr(Ast), [';'].
instruction(if(LAst, TAst)) --> [keyword(if)], logic_expr(LAst), [keyword(then)], program(TAst), [keyword(fi)], !.
instruction(if(LAst, TAst, EAst)) --> [keyword(if)], logic_expr(LAst), [keyword(then)], program(TAst), [keyword(else)], program(EAst), [keyword(fi)].
instruction(while(LAst, BAst)) --> [keyword(while)], !, logic_expr(LAst), [keyword(do)], program(BAst), [keyword(od)].

% Logic

lrec(logic_expr, logic_summand, keyword(or), logic_op(or)).

% logic_expr(Ast) --> logic_summand(Ast1), logic_expr1(Ast1, Ast).
% logic_expr1(Ast1, Ast) --> [keyword(or)], !, logic_summand(Ast2), logic_expr1(logic_op(or, Ast1, Ast2), Ast).
% logic_expr1(Ast, Ast) --> [].

% lrec(logic_summand, logic_multiplicand, keyword(and), logic_op(and)).

logic_summand(Ast) --> logic_multiplicand(Ast1), logic_summand1(Ast1, Ast).
logic_summand1(Ast1, Ast) --> [keyword(and)], !, logic_multiplicand(Ast2), logic_summand1(logic_op(and, Ast1, Ast2), Ast).
logic_summand1(Ast, Ast) --> [].

logic_multiplicand(logic_op(not, Ast)) --> [keyword(not)], !, logic_multiplicand(Ast).
logic_multiplicand(Ast) --> rel_expr(Ast).

rel_expr(Ast) --> ['('], !, logic_expr(Ast), [')'].
rel_expr(rel_op(Op, Ast1, Ast2)) --> arith_expr(Ast1), rel_op(Op), arith_expr(Ast2).

rel_op(X) --> [X], { member(X, ['=', '<', '>', '<=', '>=', '<>']), !}.

% Arithmetic

% lrec(arith_expr, arith_summand, summ_op(Op), arith_op(Op)).

arith_expr(Ast) --> arith_summand(Ast1), arith_expr1(Ast1, Ast).
arith_expr1(Ast1, Ast) --> summ_op(Op), !, arith_summand(Ast2), arith_expr1(arith_op(Op, Ast1, Ast2), Ast).
arith_expr1(Ast, Ast) --> [].

% lrec(arith_summand, arith_multiplicand, mult_op(Op), arith_op(Op)).

arith_summand(Ast) --> arith_multiplicand(Ast1), arith_summand1(Ast1, Ast).
arith_summand1(Ast1, Ast) --> mult_op(Op), !, arith_multiplicand(Ast2), arith_summand1(arith_op(Op, Ast1, Ast2), Ast).
arith_summand1(Ast, Ast) --> [].

arith_multiplicand(arith_op(^, Ast1, Ast2)) --> simple_expr(Ast1), ['^'], !, arith_multiplicand(Ast2).
arith_multiplicand(Ast) --> simple_expr(Ast).

simple_expr(Ast) --> ['('], !, arith_expr(Ast), [')'].
simple_expr(integer(N)) --> [integer(N)], !.
simple_expr(ident(I, Content)) --> [ident(I)], ['['], arith_expr(Content), [']'], !.
simple_expr(ident(I)) --> [ident(I)], !.

summ_op(X) --> [X], { member(X, ['+', '-']), ! }.
% mult_op(X) --> [X], { member(X, ['*', keyword(div), keyword(mod)]), ! }.
mult_op('*') --> ['*'], !.
mult_op('div') --> [keyword('div')], !.
mult_op('mod') --> [keyword('mod')].

parse_tokens(Tokens, Ast) :-
    phrase(parser(Ast), Tokens).

parse(Str, Tokens, Ast) :-
    lex(Str, Tokens),
    phrase(parser(Ast), Tokens).

parse_file(Path, Tokens, Ast) :-
    read_file_to_string(Path, Str, []),
    parse(Str, Tokens, Ast).

%%% -------------------------------- Task 06 ------------------------------- %%%

ident_value([(Var, Val)|_], Var, Val) :- !.
ident_value([(_, _)|Mem], Var, Val) :-
    ident_value(Mem, Var, Val).
ident_value([], _, 0).

store_ident([(Var, _)|Mem], Var, Val, [(Var, Val)|Mem]) :- !.
store_ident([(K, V)|Mem], Var, Val, [(K, V)|Mem1]) :-
    store_ident(Mem, Var, Val, Mem1).
store_ident([], Var, Val, [(Var, Val)]).

evalExpr(integer(N), _, N).
evalExpr(ident(N), Mem, Val) :-
    ident_value(Mem, N, Val).
evalExpr(ident(Arr, Content), Mem, Val) :-
    evalExpr(Content, Mem, NewContent),
    atom_concat(Arr, NewContent, NewIdent),
    ident_value(Mem, NewIdent, Val).

evalExpr(arith_op(Op, Arg1, Arg2), Mem, Val) :-
    evalExpr(Arg1, Mem, Val1),
    evalExpr(Arg2, Mem, Val2),
    Expr =.. [Op, Val1, Val2],
    call(is, Val, Expr).

evalLog(logic_op(and, Arg1, Arg2), Mem, Val) :-
    evalLog(Arg1, Mem, Val1),
    evalLog(Arg2, Mem, Val2),
    (Val1 = true, Val2 = true) -> Val = true; Val = false.
evalLog(logic_op(or, Arg1, Arg2), Mem, Val) :-
    evalLog(Arg1, Mem, Val1),
    evalLog(Arg2, Mem, Val2),
    (Val1 = true; Val2 = true) -> Val = true; Val = false.
evalLog(logic_op(not, Arg), Mem, Val) :-
    evalLog(Arg, Mem, Val),
    (Val = true) -> Val = false; Val = true.

evalLog(rel_op(Op, Arg1, Arg2), Mem, Val) :-
    evalExpr(Arg1, Mem, Val1),
    evalExpr(Arg2, Mem, Val2),
    call(Op, Val1, Val2) -> Val = true; Val = false.

evalProg([], Mem, Mem).
evalProg([S|Ss], Mem, MemOut) :-
    evalProg(S, Mem, Mem1),
    evalProg(Ss, Mem1, MemOut).

evalProg(ident(I, Idx) := Expr, Mem, MemOut) :-
    !,
    evalExpr(Idx, Mem, IdxVal),
    evalExpr(Expr, Mem, Val),
    atom_concat(I, IdxVal, NewI),
    store_ident(Mem, NewI, Val, MemOut).

% evalProg(ident("x") := _, Mem, _) :-
%     writeln(Mem),
%     fail.

evalProg(ident(I) := Expr, Mem, MemOut) :-
    evalExpr(Expr, Mem, Val),
    store_ident(Mem, I, Val, MemOut).

evalProg(if(Cond, Then), Mem, MemOut) :-
    evalLog(Cond, Mem, CondVal),
    CondVal = true -> evalProg(Then, Mem, MemOut); MemOut = Mem.

evalProg(if(Cond, Then, Else), Mem, MemOut) :-
    evalLog(Cond, Mem, CondVal),
    CondVal = true -> evalProg(Then, Mem, MemOut); 
                      evalProg(Else, Mem, MemOut).

evalProg(while(Cond, _), Mem, Mem) :-
    evalLog(Cond, Mem, false), !.

evalProg(while(Cond, Body), Mem, MemOut) :-
    evalLog(Cond, Mem, true),
    evalProg(Body, Mem, Mem1),
    evalProg(while(Cond, Body), Mem1, MemOut).

emptyInterpMemory([]).

interp(Str, Val) :-
    parse(Str, _, Ast),
    evalProg(Ast, [], Val).

interpWithMemory(Str, Memory, Val) :-
    parse(Str, _, Ast),
    evalProg(Ast, Memory, Val).

interp_file(Path, Val) :-
    parse_file(Path, _, Ast),
    evalProg(Ast, [], Val).
