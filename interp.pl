:- consult(lexer).
:- consult(parser).
:- consult(helpers).

ident_value([(Var, Val)|_], Var, Val) :- !.
ident_value([(_, _)|Mem], Var, Val) :-
    ident_value(Mem, Var, Val).
ident_value([], Var, _) :-
    domain_error("Undefined variable", Var),
    fail.

update_ident([(Var, _)|Mem], Var, Val, [(Var, Val)|Mem]) :- !.
update_ident([(K, V)|Mem], Var, Val, [(K, V)|Mem1]) :-
    update_ident(Mem, Var, Val, Mem1).
update_ident([], Var, _, _) :-
    domain_error("Undefined variable", Var),
    fail.

new_ident(Xs, Var, Val, [(Var, Val)|Xs]).

evalExpr(integer(N), _, N).
evalExpr(ident(N), Mem, Val) :-
    ident_value(Mem, N, Val).
evalExpr(ident(I, SubExpr), Mem, Val) :-
    evalExpr(SubExpr, Mem, Sub),
    ident_value(Mem, I, Arr),
    catch(arr_get(Arr, Sub, Val), "List too short!", throw(("Array", I, Sub, "too short!"))),
    !.

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

% evalProg(ident(I, SubExpr) := Expr, Mem, MemOut) :-
%     !,
%     evalExpr(SubExpr, Mem, SubExprVal),
%     evalExpr(Expr, Mem, Val),
%     atom_concat(I, SubExprVal, NewI),
%     update_ident(Mem, NewI, Val, MemOut).

evalProg(def_value(I, Expr), Mem, MemOut) :-
    evalExpr(Expr, Mem, Val),
    new_ident(Mem, I, Val, MemOut).

evalProg(def_array(I, LengthExpr, ArrayExprs), Mem, MemOut) :-
    evalExpr(LengthExpr, Mem, Length),
    parallelEval(ArrayExprs, evalExpr, Mem, ArrayVals),
    length(Val, Length),
    repeatingList(Val, ArrayVals),
    new_ident(Mem, I, array(Length, Val), MemOut).

evalProg(assignment(I, Expr), Mem, MemOut) :-
    evalExpr(Expr, Mem, Val),
    update_ident(Mem, I, Val, MemOut).

evalProg(assignment(I, SubExpr, ValExpr), Mem, MemOut) :-
    parallelEval([ValExpr, SubExpr], evalExpr, Mem, [Val, Sub]),
    ident_value(Mem, I, Arr),
    catch(arr_set(Arr, Sub, Val, NewArr), "List too short!", throw(("Array", I, Sub, "too short!"))),
    !,
    update_ident(Mem, I, NewArr, MemOut).

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
