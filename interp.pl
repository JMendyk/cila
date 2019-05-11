:- consult(lexer).
:- consult(parser).
:- consult(helpers).
:- consult(env_helpers).

evalExpr(integer(N), _, N).
evalExpr(ident(N), Env, Val) :-
    getVar(Env, (N, Val)).
evalExpr(ident(I, SubExpr), Env, Val) :-
    evalExpr(SubExpr, Env, Sub),
    getVar(Env, (I, Arr)),
    catch(arr_get(Arr, Sub, Val), "List too short!", throw(("Array", I, Sub, "too short!"))),
    !.

evalExpr(arith_op(Op, Arg1, Arg2), Env, Val) :-
    evalExpr(Arg1, Env, Val1),
    evalExpr(Arg2, Env, Val2),
    Expr =.. [Op, Val1, Val2],
    call(is, Val, Expr).

evalLog(logic_op(and, Arg1, Arg2), Env, Val) :-
    evalLog(Arg1, Env, Val1),
    evalLog(Arg2, Env, Val2),
    ((Val1 = true, Val2 = true) -> (Val = true, !); Val = false),
    !.
evalLog(logic_op(or, Arg1, Arg2), Env, Val) :-
    evalLog(Arg1, Env, Val1),
    evalLog(Arg2, Env, Val2),
    ((Val1 = true; Val2 = true) -> (Val = true, !); Val = false),
    !.
evalLog(logic_op(not, Arg), Env, Val) :-
    evalLog(Arg, Env, Val),
    ((Val = true) -> (Val = false, !); Val = true),
    !.

evalLog(rel_op(=, Arg1, Arg2), Env, Val) :- !,
    evalExpr(Arg1, Env, Val1),
    evalExpr(Arg2, Env, Val2),
    (Val1 == Val2 -> (Val = true, !); Val = false),
    !.

evalLog(rel_op(<>, Arg1, Arg2), Env, Val) :- !,
    evalExpr(Arg1, Env, Val1),
    evalExpr(Arg2, Env, Val2),
    ((\+ Val1 == Val2) -> (Val = true, !); Val = false),
    !.

evalLog(rel_op(<=, Arg1, Arg2), Env, Val) :- !,
    evalExpr(Arg1, Env, Val1),
    evalExpr(Arg2, Env, Val2),
    (Val1 =< Val2 -> (Val = true, !); Val = false),
    !.

evalLog(rel_op(Op, Arg1, Arg2), Env, Val) :-
    \+ member(Op, ['<=', '<>', '=']), % Don't know why this is required...
    evalExpr(Arg1, Env, Val1),
    evalExpr(Arg2, Env, Val2),
    (call(Op, Val1, Val2) -> (Val = true, !); Val = false),
    !.

evalProg(Ls, Env, EnvOut) :-
    is_list(Ls),
    !,
    sequentialEval(Ls, evalProg, Env, EnvOut).

evalProg(def_value(I, Expr), Mem, MemOut) :-
    evalExpr(Expr, Mem, Val),
    createVar(Mem, MemOut, (I, Val)).

evalProg(def_array(I, LengthExpr, ArrayExprs), Mem, MemOut) :-
    evalExpr(LengthExpr, Mem, Length),
    parallelEval(ArrayExprs, evalExpr, Mem, ArrayVals),
    length(Val, Length),
    repeatingList(Val, ArrayVals),
    createVar(Mem, MemOut, (I, array(Length, Val))).

evalProg(assignment(I, Expr), Mem, MemOut) :-
    evalExpr(Expr, Mem, Val),
    setVar(Mem, MemOut, (I, Val)).

evalProg(assignment(I, SubExpr, ValExpr), Mem, MemOut) :-
    parallelEval([ValExpr, SubExpr], evalExpr, Mem, [Val, Sub]),
    getVar(Mem, (I, Arr)),
    catch(arr_set(Arr, Sub, Val, NewArr), "List too short!", throw(("Array", I, Sub, "too short!"))),
    !,
    setVar(Mem, MemOut, (I, NewArr)).

evalProg(if(Cond, Then), Mem, MemOut) :-
    createScope(Mem, Mem1),
    evalLog(Cond, Mem1, CondVal),
    evalBranch(CondVal, evalProg, Then, [], Mem1, Mem2),
    destroyScope(Mem2, MemOut).

evalProg(if(Cond, Then, Else), Mem, MemOut) :-
    createScope(Mem, Mem1),
    evalLog(Cond, Mem1, CondVal),
    evalBranch(CondVal, evalProg, Then, Else, Mem1, Mem2),
    destroyScope(Mem2, MemOut).

evalProg(while(Cond, Body), Mem, MemOut) :-
    createScope(Mem, Mem1),
    evalLog(Cond, Mem1, CondVal),
    evalBranch(CondVal, evalProg, [Body, while(Cond, Body)], [], Mem1, Mem2),
    destroyScope(Mem2, MemOut).


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
    initEnv(Env),
    evalProg(Ast, Env, Memory).

interp_file(Path, Memory) :-
    read_file_to_string(Path, Str, []),
    lex(Str, Tokens),
    parse(Tokens, Ast),
    interp(Ast, Memory).
