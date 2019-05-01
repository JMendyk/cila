:- consult(lexer).
:- consult(parser).

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
    (Val1 = true, Val2 = true) -> (Val = true, !); Val = false.
evalLog(logic_op(or, Arg1, Arg2), Mem, Val) :-
    evalLog(Arg1, Mem, Val1),
    evalLog(Arg2, Mem, Val2),
    (Val1 = true; Val2 = true) -> (Val = true, !); Val = false.
evalLog(logic_op(not, Arg), Mem, Val) :-
    evalLog(Arg, Mem, Val),
    (Val = true) -> (Val = false, !); Val = true.

evalLog(rel_op(=, Arg1, Arg2), Mem, Val) :- !,
    evalExpr(Arg1, Mem, Val1),
    evalExpr(Arg2, Mem, Val2),
    Val1 == Val2 -> Val = true; Val = false.

evalLog(rel_op(<>, Arg1, Arg2), Mem, Val) :- !,
    evalExpr(Arg1, Mem, Val1),
    evalExpr(Arg2, Mem, Val2),
    (\+ Val1 == Val2) -> Val = true; Val = false.

evalLog(rel_op(<=, Arg1, Arg2), Mem, Val) :- !,
    evalExpr(Arg1, Mem, Val1),
    evalExpr(Arg2, Mem, Val2),
    Val1 =< Val2 -> Val = true; Val = false.

evalLog(rel_op(Op, Arg1, Arg2), Mem, Val) :-
    \+ member(Op, ['<=', '<>', '=']), % Don't know why this is required...
    evalExpr(Arg1, Mem, Val1),
    evalExpr(Arg2, Mem, Val2),
    call(Op, Val1, Val2) -> Val = true; Val = false.

evalProg([], Mem, Mem) :- !.
evalProg([S|Ss], Mem, MemOut) :-
    evalProg(S, Mem, Mem1),
    evalProg(Ss, Mem1, MemOut).

evalProg(ident(I, Idx) := Expr, Mem, MemOut) :-
    !,
    evalExpr(Idx, Mem, IdxVal),
    evalExpr(Expr, Mem, Val),
    atom_concat(I, IdxVal, NewI),
    store_ident(Mem, NewI, Val, MemOut).

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

evalProg(while(Cond, Body), Mem, MemOut) :-
    evalLog(Cond, Mem, CondVal),
    CondVal = true -> (evalProg(Body, Mem, Mem1), evalProg(while(Cond, Body), Mem1, MemOut)); MemOut = Mem.

emptyInterpMemory([]).

% interp(Str, Val) :-
%     parse(Str, _, Ast),
%     evalProg(Ast, [], Val).

% interpWithMemory(Str, Memory, Val) :-
%     parse(Str, _, Ast),
%     evalProg(Ast, Memory, Val).

% interp_file(Path, Val) :-
%     parse_file(Path, _, Ast),
%     evalProg(Ast, [], Val).

interp(Ast, Memory) :-
    evalProg(Ast, [], Memory).

interp_file(Path, Memory) :-
    read_file_to_string(Path, Str, []),
    lex(Str, Tokens),
    parse(Tokens, Ast),
    interp(Ast, Memory).
