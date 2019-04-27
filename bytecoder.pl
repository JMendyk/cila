:- consult(lexer).
:- consult(parser).

% Machine:
% 8 registers r0-r7 (r0 being Accumulator), Stack

% BYTECODES:
% load_const - load constant to Accumulator
% clear - set Accumulator to zero
% mov(rX) - move r0 content to rX
% mov(rA, rB) - move rX content to rY
% push(rX) - store rX's value on top of stack
% pop(rX)  - load stack's top value into rX
% pop   - forget top value in stack
% reach(offset, rX) - load value from stack at given offset into rX
% throw(rX, offset) - store value from rX into stack at given offset
% add, sub, mult, div, mod - arithmetic operations working on r1 and r2 as args and storing result in r0
% or, and, not - logic operations working on r1 and r2 as args and storing result in r0
% jump - unconditional jump
% jz   - jump if Accumulator is equal to zero
% jgtz - jump if Accumulator is greater or equal zero

get_head(T, T, T).

bytecode(integer(N), Vars, Vars) --> [load_const(N)].
bytecode(arith_op('+', Arg1, Arg2), InVs, InVs) --> !, bytecode(Arg1, InVs, InVs), [mov(r1)], bytecode(Arg2, InVs, InVs), [mov(r2)], [add].
bytecode(arith_op('-', Arg1, Arg2), InVs, InVs) --> !, bytecode(Arg1, InVs, InVs), [mov(r1)], bytecode(Arg2, InVs, InVs), [mov(r2)], [sub].
bytecode(arith_op('*', Arg1, Arg2), InVs, InVs) --> !, bytecode(Arg1, InVs, InVs), [mov(r1)], bytecode(Arg2, InVs, InVs), [mov(r2)], [mult].
bytecode(arith_op('div', Arg1, Arg2), InVs, InVs) --> !, bytecode(Arg1, InVs, InVs), [mov(r1)], bytecode(Arg2, InVs, InVs), [mov(r2)], [div].
bytecode(arith_op('mod', Arg1, Arg2), InVs, InVs) --> !, bytecode(Arg1, InVs, InVs), [mov(r1)], bytecode(Arg2, InVs, InVs), [mov(r2)], [mod].

bytecode(if(Cond, Then), InVs, OutVs) --> !, 
    bytecode(Cond, InVs, Vs1), [jz(EndLabel)], 
    bytecode(Then, Vs1, OutVs), 
    get_head(EndLabel).

bytecode(if(Cond, Then, Else), InVs, OutVs) --> !, 
    bytecode(Cond, InVs, Vs1), [jz(ElseLabel)], 
    bytecode(Then, Vs1, Vs2), [jump(EndLabel)], 
    get_head(ElseLabel), bytecode(Else, Vs2, OutVs),
    get_head(EndLabel).

bytecode(while(Cond, Body), InVs, OutVs) --> !, 
    get_head(CondLabel),
    bytecode(Cond, InVs, InVs),
    [jz(EndLabel)],
    bytecode(Body, InVs, OutVs),
    [jump(CondLabel)],
    get_head(EndLabel).

bytecode(logic_op(and, Arg1, Arg2), InVs, InVs) --> !,
    bytecode(Arg1, InVs, InVs), [mov(r1)], bytecode(Arg2, InVs, InVs), [mov(r2)], [and].
bytecode(logic_op(or, Arg1, Arg2), InVs, InVs) --> !,
    bytecode(Arg1, InVs, InVs), [mov(r1)], bytecode(Arg2, InVs, InVs), [mov(r2)], [or].
bytecode(logic_op(not, Arg), InVs, InVs) --> !,
    bytecode(Arg, InVs, InVs), [store], [not], [pop].

bytecode(rel_op('=', Arg1, Arg2), InVs, InVs) --> !,
    bytecode(Arg1, InVs, InVs), [mov(r1)], bytecode(Arg2, InVs, InVs), [mov(r2)], [clear, set_equal].
bytecode(rel_op('<>', Arg1, Arg2), InVs, InVs) --> !,
    bytecode(Arg1, InVs, InVs), [mov(r1)], bytecode(Arg2, InVs, InVs), [mov(r2)], [clear, set_not_equal].
bytecode(rel_op('<', Arg1, Arg2), InVs, InVs) --> !,
    bytecode(Arg1, InVs, InVs), [mov(r1)], bytecode(Arg2, InVs, InVs), [mov(r2)], [clear, set_lt].
bytecode(rel_op('>', Arg1, Arg2), InVs, InVs) --> !,
    bytecode(Arg2, InVs, InVs), [mov(r1)], bytecode(Arg1, InVs, InVs), [mov(r2)], [clear, set_lt].

bytecode(rel_op('<=', Arg1, Arg2), InVs, InVs) --> !,
    bytecode(Arg1, InVs, InVs), [mov(r1)], bytecode(Arg2, InVs, InVs), [mov(r2)], [clear, set_equal, set_lt].
bytecode(rel_op('>=', Arg1, Arg2), InVs, InVs) --> !,
    bytecode(Arg2, InVs, InVs), [mov(r1)], bytecode(Arg1, InVs, InVs), [mov(r2)], [clear, set_equal, set_lt].

bytecode([], Vars, Vars) --> !.
bytecode([C|Cs], InVs, OutVs) --> bytecode(C, InVs, Vs1), bytecode(Cs, Vs1, OutVs).

bytecode(ident(I), InVs, InVs) --> { offset_of(I, InVs, Offset) }, [reach(Offset, r0)].

bytecode(ident(I) := Expr, InVs, InVs) --> { member(I, InVs) }, !, bytecode(Expr, InVs, InVs), { offset_of(I, InVs, Offset) }, [throw(r0, Offset)].

bytecode(ident(I) := Expr, InVs, [I|InVs]) --> !, bytecode(Expr, InVs, InVs), [push(r0)], { write("Defining variable "), writeln(I) }.

offset_of(I, [I|_], 0) :- !.
offset_of(I, [_|Vs], N) :-
    offset_of(I, Vs, N1),
    N is N1 + 1.

% emptyInterpMemory([]).

bytecode_of(Ast, Bytecode) :-
    phrase(bytecode(Ast, [], _), Bytecode, []).

bytecode_as_string(Ast, List) :-
    bytecode_of(Ast, Bytecode),
    maplist(term_to_atom, Bytecode, L), atomic_list_concat(L, '\n', List).



% byteExpr(ident(Arr, Content), Mem, Val) :-
%     byteExpr(Content, Mem, NewContent),
%     atom_concat(Arr, NewContent, NewIdent),
%     ident_value(Mem, NewIdent, Val).

% bytecode(ident(I, Idx) := Expr, InVs, OutVs) -->
%     { atom_concat(I, IdxVal, NewI) }, !, bytecode(NewI := Expr, InVs, OutVs).