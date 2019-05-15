% Optional and sequence sourced from http://www.swi-prolog.org/pldoc/doc/_SWI_/library/dcg/high_order.pl

/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018, VU University Amsterdam
                         CWI, Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
    */

optional(Match, _) -->
    Match, !.
optional(_, Default) -->
    Default, !.

sequence(Start, OnElem, OnSep, End, List) -->
    Start,
    sequence_(List, OnElem, OnSep),
    End, !.

sequence_([H|T], P, Sep) -->
    call(P, H),
    (   {T == []}
    ->  []
    ;   Sep
    ->  !, sequence_(T, P, Sep)
    ;   {T = []}
    ).
sequence_([], _, _) -->
    [].

% End of sourced predicated from dcg/high_order

term_expansion(left_recursion(Pred, Sngl, Sep, Combine), [
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

keyword(K) --> [keyword(K, _)].
keyword(K, CL) --> [keyword(K, CL)].
char(C) --> [char(C, _)].
char(C, CL) --> [char(C, CL)].
ident(I) --> [ident(I, _)].
ident(I, CL) --> [ident(I, CL)].
integer(I) --> [integer(I, _)].
integer(I, CL) --> [integer(I, CL)].
string(I) --> [string(I, _)].
string(I, CL) --> [string(I, CL)].
parser(Ast) --> program(Ast).

array_expr(array(Length, Values)) -->
    optional((char('['), optional(arith_expr(Length), []), char(']')), []),
    sequence(char('{'), arith_expr, char(','), char('}'), Values),
    { nonvar(Length); (length(Values, Length1), Length = integer(Length1)) }.

identName(ident(I), I).

program([Ast|Asts]) --> instruction(Ast), !, program(Asts).
program([]) --> [].

instruction(Ast) --> definition(Ast), !.
instruction(Ast) --> assign_inst(Ast), !.
instruction(Ast) --> if_inst(Ast), !.
instruction(Ast) --> while_inst(Ast), !.
instruction(Ast) --> call_inst(Ast), char(';'), !.

value(arith(call(Fun, Args))) --> 
    call_inst(call(Fun, Args)),
    !.
value(arith(Ast)) --> arith_expr(Ast).
value(string(Content)) --> string(Content).
value(Ast) --> array_expr(Ast).
value(boolean(Ast)) --> logic_expr(Ast),
    { atomic_list_concat(["Attempted to introduce boolean value.\n", Ast, "Boolean expressions are not supported as values."], Message),
    writeln(Message),
    throw(Message) }.
value(fun(ArgNames, Body)) --> 
    keyword(fun),
    !,
    sequence(char('('), ident, char(','), char(')'), ArgNames),
    program(Body),
    keyword(nuf).

definition(def(I, Value)) -->
    keyword(let),
    ident(I),
    char(':='),
    !,
    value(Value),
    char(';').

assign_inst(assignment(I, Ast)) --> 
    ident(I), 
    char(':='),
    value(arith(Ast)),
    char(';').

assign_inst(assignment(I, Sub, Ast)) --> 
    ident(I), 
    char('['), value(arith(Sub)), char(']'),
    char(':='),
    value(arith(Ast)), char(';').

if_inst(Ast) -->
    keyword(if), logic_expr(Cond),
    keyword(then), program(Then),
    optional((keyword(else), program(Else), { Ast = if(Cond, Then, Else) }),
    { Ast = if(Cond, Then) }),
    keyword(fi).

while_inst(while(Cond, Body)) --> keyword(while), !, logic_expr(Cond), keyword(do), program(Body), keyword(od).

call_inst(call(I, Args)) -->
    ident(I),
    sequence(char('('), arith_expr_or_value, char(','), char(')'), Args),
    !.

arith_expr_or_value(Ast) -->
    arith_expr(Ast), !.

arith_expr_or_value(Ast) -->
    value(Ast).

% Logic

left_recursion(logic_expr, logic_summand, keyword(or), logic_op(or)).

left_recursion(logic_summand, logic_multiplicand, keyword(and), logic_op(and)).

logic_multiplicand(logic_op(not, Ast)) --> keyword(not), !, logic_multiplicand(Ast).
logic_multiplicand(Ast) --> rel_expr(Ast).

rel_expr(Ast) --> char('('), !, logic_expr(Ast), char(')').

rel_expr(rel_op(Op, Ast1, Ast2)) --> arith_expr(Ast1), rel_op(Op), arith_expr(Ast2).

rel_op(X) --> char(X), { member(X, ['=', '<', '>', '<=', '>=', '<>']), !}.

% Arithmetic

left_recursion(arith_expr, arith_summand, summ_op(Op), arith_op(Op)).

left_recursion(arith_summand, arith_multiplicand, mult_op(Op), arith_op(Op)).

arith_multiplicand(arith_op(^, Ast1, Ast2)) --> simple_expr(Ast1), char('^'), !, arith_multiplicand(Ast2).
arith_multiplicand(Ast) --> simple_expr(Ast).

simple_expr(Ast) --> char('('), !, value(arith(Ast)), char(')').
simple_expr(integer(N)) --> integer(N), !.
simple_expr(Call) --> call_inst(Call), !.
simple_expr(ident(I, Content)) --> ident(I), char('['), arith_expr(Content), char(']'), !.
simple_expr(ident(I)) --> ident(I), !.

summ_op(X) --> char(X), { member(X, ['+', '-']), ! }.
% mult_op(X) --> char(X), { member(X, ['*', keyword(div), keyword(mod)]), ! }.
mult_op('*') --> char('*'), !.
mult_op(div) --> keyword(div), !.
mult_op(mod) --> keyword(mod).

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
