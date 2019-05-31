:- consult(lexer).
:- consult(parser).
:- ensure_loaded(helpers).

% Note: 

get_head(T, T, T).

bytecode(integer(N), Vars, Vars) --> [load_int(N)].
bytecode(arith_op('+', Arg1, Arg2), InVs, OutVs) --> !, 
    bytecode(Arg2, InVs, Vs1), bytecode(Arg1, Vs1, OutVs), [add].
bytecode(arith_op('-', Arg1, Arg2), InVs, OutVs) --> !, 
    bytecode(Arg2, InVs, Vs1), bytecode(Arg1, Vs1, OutVs), [sub].
bytecode(arith_op('*', Arg1, Arg2), InVs, OutVs) --> !, 
    bytecode(Arg2, InVs, Vs1), bytecode(Arg1, Vs1, OutVs), [mult].
bytecode(arith_op('div', Arg1, Arg2), InVs, OutVs) --> !, 
    bytecode(Arg2, InVs, Vs1), bytecode(Arg1, Vs1, OutVs), [div].
bytecode(arith_op('mod', Arg1, Arg2), InVs, OutVs) --> !, 
    bytecode(Arg2, InVs, Vs1), bytecode(Arg1, Vs1, OutVs), [mod].

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
    bytecode(Cond, InVs, Vs1),
    [jz(EndLabel)],
    bytecode(Body, Vs1, OutVs),
    [jump(CondLabel)],
    get_head(EndLabel).

bytecode(logic_op(and, Arg1, Arg2), InVs, OutVs) --> !,
    bytecode(Arg2, InVs, Vs1), bytecode(Arg1, Vs1, OutVs), [and].
bytecode(logic_op(or, Arg1, Arg2), InVs, OutVs) --> !,
    bytecode(Arg2, InVs, Vs1), bytecode(Arg1, Vs1, OutVs), [or].
bytecode(logic_op(not, Arg), InVs, OutVs) --> !,
    bytecode(Arg, InVs, OutVs), [not].

bytecode(rel_op('=', Arg1, Arg2), InVs, OutVs) --> !,
    bytecode(Arg2, InVs, Vs1), bytecode(Arg1, Vs1, OutVs), [equal].
bytecode(rel_op('<>', Arg1, Arg2), InVs, OutVs) --> !,
    bytecode(Arg2, InVs, Vs1), bytecode(Arg1, Vs1, OutVs), [equal, not].
bytecode(rel_op('<', Arg1, Arg2), InVs, OutVs) --> !,
    bytecode(Arg2, InVs, Vs1), bytecode(Arg1, Vs1, OutVs), [lt].
bytecode(rel_op('>', Arg1, Arg2), InVs, OutVs) --> !,
    bytecode(Arg1, InVs, Vs1), bytecode(Arg2, Vs1, OutVs), [lt].

bytecode(rel_op('<=', Arg1, Arg2), InVs, OutVs) --> !,
    bytecode(Arg2, InVs, Vs1), bytecode(Arg1, Vs1, OutVs), [leq].
bytecode(rel_op('>=', Arg1, Arg2), InVs, OutVs) --> !,
    bytecode(Arg1, InVs, Vs1), bytecode(Arg2, Vs1, OutVs), [leq].

bytecode([], Vars, Vars) --> !.
bytecode([C|Cs], InVs, OutVs) --> bytecode(C, InVs, Vs1), bytecode(Cs, Vs1, OutVs).

bytecode(ident(I), InVs, OutVs) --> { code_of(I, InVs, OutVs, Code) }, [load(Code)].

bytecode(def(I, Expr), InVs, OutVs) --> !, 
    bytecode(Expr, InVs, Vs1), { code_of(I, Vs1, OutVs, Code) }, [store(Code)].

bytecode(assignment(I, Expr), InVs, OutVs) --> !, 
    bytecode(Expr, InVs, Vs1), { code_of(I, Vs1, OutVs, Code) }, [store(Code)].

bytecode(X, _, _) --> { writeln(X), fail }.

code_of(I, Vs, Out, Code) :-
    code_of(0, I, Vs, Out, Code).

code_of(N, I, [], [I], N) :- !.
code_of(N, I, [I|Is], [I|Is], N) :- !.
code_of(N, I, [X|Is], [X|Is1], Code) :-
    N1 is N + 1,
    code_of(N1, I, Is, Is1, Code).

bytecode_of(Ast, (Bytecode, Mappings)) :-
    phrase(bytecode(Ast, [], Mappings), Bytecode, []),
    !.

bytecode_of(_, _) :-
    writeln("Conversion to bytecode failed").

bytecode_as_string(Ast, (List, Mappings)) :-
    bytecode_of(Ast, (Bytecode, Mappings)),
    maplist(term_to_atom, Bytecode, L), atomic_list_concat(L, '\n', List).



% byteExpr(ident(Arr, Content), Mem, Val) :-
%     byteExpr(Content, Mem, NewContent),
%     atom_concat(Arr, NewContent, NewIdent),
%     ident_value(Mem, NewIdent, Val).

% bytecode(ident(I, Idx) := Expr, InVs, OutVs) -->
%     { atom_concat(I, IdxVal, NewI) }, !, bytecode(NewI := Expr, InVs, OutVs).