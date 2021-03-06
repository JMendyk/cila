:- ensure_loaded(lexer).
:- ensure_loaded(parser).
:- ensure_loaded(helpers).
:- ensure_loaded(env_helpers).

arity_mismatch(Fun, closure(RealFunName, ArgNames, _), Args) :-
    length(ArgNames, Expected),
    length(Args, Got),
    any_list_concat(["Arity mismatch while calling ", Fun, " (real name ", RealFunName, "). Function expected ", Expected, " arguments, got ", Got, "."], Msg),
    writeln(Msg),
    throw(arity_mismatch(Fun, Msg)).

evalExpr(integer(N), _, N).
evalExpr(ident(N), Env, Val) :-
    getVar(Env, (N, Val)).
evalExpr(ident(I, SubExpr), Env, Val) :-
    evalExpr(SubExpr, Env, Sub),
    getVar(Env, (I, Arr)),
    catch(arr_get(Arr, Sub, Val), "List too short!", throw(("Array", I, Sub, "too short!"))),
    !.
evalExpr(call("read", []), Env, Val) :-
    read_line_to_string(user_input, Str),
    atom_number(Str, X),
    evalExpr(integer(X), Env, Val),
    !.
evalExpr(call(Fun, ArgExprs), Env, Val) :-
    parallelEval(ArgExprs, evalExpr, Env, Args),
    getVar(Env, (Fun, closure(RealFunName, ArgNames, Body))),
    (same_length(Args, ArgNames); arity_mismatch(Fun, closure(RealFunName, ArgNames, Body), Args)),
    !,
    callArgs(ArgNames, Args, CallArgs),
    createCall(Env, Env1, CallArgs),
    createVar(Env1, Env2, (RealFunName, closure(RealFunName, ArgNames, Body))),
    evalProg(Body, Env2, Env3),
    checkReturnValue(Env3, Val),
    destroyCall(Env3, Env),
    !.

evalExpr(arith_op(Op, Arg1, Arg2), Env, Val) :-
    evalExpr(Arg1, Env, Val1),
    evalExpr(Arg2, Env, Val2),
    Expr =.. [Op, Val1, Val2],
    call(is, Val, Expr).

evalLog(logic_op(and, Arg1, Arg2), Env, Val) :- !,
    evalLog(Arg1, Env, Val1),
    evalLog(Arg2, Env, Val2),
    ((Val1 = true, Val2 = true) -> (Val = true, !); Val = false).
evalLog(logic_op(or, Arg1, Arg2), Env, Val) :- !,
    evalLog(Arg1, Env, Val1),
    evalLog(Arg2, Env, Val2),
    ((Val1 = true; Val2 = true) -> (Val = true, !); Val = false).
evalLog(logic_op(not, Arg), Env, Val) :-
    evalLog(Arg, Env, Val),
    ((Val = true) -> (Val = false, !); Val = true).

evalLog(rel_op(=, Arg1, Arg2), Env, Val) :- !,
    evalExpr(Arg1, Env, Val1),
    evalExpr(Arg2, Env, Val2),
    (Val1 == Val2 -> (Val = true, !); Val = false).

evalLog(rel_op(<>, Arg1, Arg2), Env, Val) :- !,
    evalExpr(Arg1, Env, Val1),
    evalExpr(Arg2, Env, Val2),
    ((\+ Val1 == Val2) -> (Val = true, !); Val = false).

evalLog(rel_op(<=, Arg1, Arg2), Env, Val) :- !,
    evalExpr(Arg1, Env, Val1),
    evalExpr(Arg2, Env, Val2),
    (Val1 =< Val2 -> (Val = true, !); Val = false).

evalLog(rel_op(Op, Arg1, Arg2), Env, Val) :-
    evalExpr(Arg1, Env, Val1),
    evalExpr(Arg2, Env, Val2),
    (call(Op, Val1, Val2) -> (Val = true, !); Val = false).

evalProg([], Env, Env) :- !.
evalProg([S|Ss], Env, EnvOut) :-
    evalProg(S, Env, Env1),
    !,
    (
        (didReturnValue(Env1), !, EnvOut = Env1);
        evalProg(Ss, Env1, EnvOut)).

evalProg(def(I, arith(Expr)), Mem, MemOut) :-
    !,
    evalExpr(Expr, Mem, Val),
    createVar(Mem, MemOut, (I, Val)).

evalProg(def(I, array(LengthExpr, ArrayExprs)), Mem, MemOut) :-
    !,
    evalExpr(LengthExpr, Mem, Length),
    parallelEval(ArrayExprs, evalExpr, Mem, ArrayVals),
    length(Val, Length),
    repeatingList(Val, ArrayVals),
    createVar(Mem, MemOut, (I, array(Length, Val))).

evalProg(def(I, fun(ArgNames, Body)), Mem, MemOut) :-
    createVar(Mem, MemOut, (I, closure(I, ArgNames, Body))).

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
    evalLog(Cond, Mem, CondVal),
    evalIf(CondVal, if(Cond, Then, []), Mem, MemOut).
evalProg(if(Cond, Then, Else), Mem, MemOut) :-
    evalLog(Cond, Mem, CondVal),
    evalIf(CondVal, if(Cond, Then, Else), Mem, MemOut).

evalProg(while(Cond, Body), Mem, MemOut) :-
    evalLog(Cond, Mem, CondVal),
    whileEval(CondVal, while(Cond, Body), Mem, MemOut).

evalProg(call("return", [ReturnExpr]), Env, Env1) :-
    !,
    evalExpr(ReturnExpr, Env, Val),
    returnValue(Env, Env1, Val).

evalProg(call("return", _), _, _) :-
    throw(arity_mismatch("Return expects exactly one argument!")).

evalProg(call("print", [string(Expr)]), Env, Env) :-
    !,
    any_list_concat(["[> ", Expr], Msg),
    writeln(Msg).

evalProg(call("print", [Expr]), Env, Env) :-
    !,
    evalExpr(Expr, Env, Val),
    any_list_concat(["[> ", Val], Msg),
    writeln(Msg).

evalProg(call("env", []), Env, Env) :-
    !,
    prettyEnv(Env, PrettyEnv),
    writeln(PrettyEnv).

evalProg(call(Fun, ArgExprs), Mem, Mem) :-
    evalExpr(call(Fun, ArgExprs), Mem, _).

callArgs(Names, Values, CallArgs) :-
    zip(Names, Values, CallArgs).


evalIf(true, if(_, Then, _), Mem, MemOut) :-
    createNamedScope(Mem, Mem1, "if"),
    evalProg(Then, Mem1, Mem2),
    destroyScope(Mem2, MemOut).

evalIf(false, if(_, _, Else), Mem, MemOut) :-
    createNamedScope(Mem, Mem1, "if"),
    evalProg(Else, Mem1, Mem2),
    destroyScope(Mem2, MemOut).

whileEval(true, while(Cond, Body), Mem, MemOut) :-
    createScope(Mem, Mem1),
    evalProg(Body, Mem1, Mem2),
    destroyScope(Mem2, Mem3),
    evalProg(while(Cond, Body), Mem3, MemOut).

whileEval(false, while(_, _), Mem, Mem).

interp(Ast, Memory) :-
    initEnv(Env),
    ((evalProg(Ast, Env, Memory), !); writeln("Interpreter failed unexpectedly.")).

interp_file(Path, Memory) :-
    read_file_to_string(Path, Str, []),
    lex(Str, Tokens),
    parse(Tokens, Ast),
    interp(Ast, Memory).
