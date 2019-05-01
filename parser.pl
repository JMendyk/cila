optional(Match, _) -->
    Match, !.
optional(_, Default) -->
    Default, !.

term_expansion(lrec(Pred, Sngl, Sep, Combine), [
    Base --> (Single1, One), 
    One --> (Sep, !, Single2, OneComb),
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

keyword(K) --> [keyword(K)].
char(C) --> [C].
ident(I) --> [ident(I)].
integer(I) --> [integer(I)].

parser(Ast) --> program(Ast).

program([Ast|Asts]) --> instruction(Ast), !, program(Asts).
program([]) --> [].

instruction(ident(I, Content) := Ast) --> ident(I), char('['), arith_expr(Content), char(']'), char(':='), !, arith_expr(Ast), char(';').
instruction(ident(I) := Ast) --> ident(I), char(':='), !, arith_expr(Ast), char(';').
instruction(Ast) --> if_expr(Ast), !.
instruction(while(LAst, BAst)) --> keyword(while), !, logic_expr(LAst), keyword(do), program(BAst), keyword(od).

if_expr(Ast) -->
    keyword(if), logic_expr(Cond),
    keyword(then), program(Then),
    optional((keyword(else), program(Else), { Ast = if(Cond, Then, Else) }),
    { Ast = if(Cond, Then) }),
    keyword(fi).

% Logic

lrec(logic_expr, logic_summand, keyword(or), logic_op(or)).

lrec(logic_summand, logic_multiplicand, keyword(and), logic_op(and)).

logic_multiplicand(logic_op(not, Ast)) --> keyword(not), !, logic_multiplicand(Ast).
logic_multiplicand(Ast) --> rel_expr(Ast).

rel_expr(Ast) --> char('('), !, logic_expr(Ast), char(')').
rel_expr(rel_op(Op, Ast1, Ast2)) --> arith_expr(Ast1), rel_op(Op), arith_expr(Ast2).

rel_op(X) --> char(X), { member(X, ['=', '<', '>', '<=', '>=', '<>']), !}.

% Arithmetic

lrec(arith_expr, arith_summand, summ_op(Op), arith_op(Op)).

lrec(arith_summand, arith_multiplicand, mult_op(Op), arith_op(Op)).

arith_multiplicand(arith_op(^, Ast1, Ast2)) --> simple_expr(Ast1), char('^'), !, arith_multiplicand(Ast2).
arith_multiplicand(Ast) --> simple_expr(Ast).

simple_expr(Ast) --> char('('), !, arith_expr(Ast), char(')').
simple_expr(integer(N)) --> integer(N), !.
simple_expr(ident(I, Content)) --> ident(I), char('['), arith_expr(Content), char(']'), !.
simple_expr(ident(I)) --> ident(I), !.

summ_op(X) --> char(X), { member(X, ['+', '-']), ! }.
% mult_op(X) --> char(X), { member(X, ['*', keyword(div), keyword(mod)]), ! }.
mult_op('*') --> char('*'), !.
mult_op('div') --> keyword('div'), !.
mult_op('mod') --> keyword('mod').

% parse_tokens(Tokens, Ast) :-
%     phrase(parser(Ast), Tokens).

% parse(Str, Tokens, Ast) :-
%     lex(Str, Tokens),
%     phrase(parser(Ast), Tokens).

% parse_file(Path, Tokens, Ast) :-
%     read_file_to_string(Path, Str, []),
%     parse(Str, Tokens, Ast).

parse(Tokens, Ast) :-
    phrase(parser(Ast), Tokens).
