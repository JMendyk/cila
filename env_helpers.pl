:- consult(helpers).

% Predicates for working with interpreter's environment

default(Var, _) :-
    nonvar(Var), !.
default(Var, Default) :-
    var(Var),
    Var = Default.

% Environment: env(GlobalMem, LocalMem, IsInGlobalScope, ExpressionValue)
initEnv(env([], [], true, _)).



isInGlobalScope(env(_, _, IsInGlobalScope, _), IsInGlobalScope).



returnValue(env(GM, LM, IG, _), env(GM, LM, IG, EV), EV).
checkReturnValue(env(_, _, _, EV), EV1) :-
    nonvar(EV),
    EV1 = EV.
didReturnValue(env(_, _, _, EV)) :- nonvar(EV).
clearReturnValue(env(GM, LM, IG, _), env(GM, LM, IG, _)).



createVar(env(GlobalMem, LM, true, EV), env(GlobalMem1, LM, true, EV), Var) :-
    createVar_(GlobalMem, GlobalMem1, Var),
    !.

createVar(env(GM, LocalMem, false, EV), env(GM, LocalMem1, false, EV), Var) :-
    createVar_(LocalMem, LocalMem1, Var).

createVar_(Mem, [(VarName, VarValue)|Mem], (VarName, VarValue)).

report(env(GlobalMem, LocalMem, _, _), get, (VarName, _, _)) :-
    write("GM --> "), prettyVariables(GlobalMem, GM), writeln(GM),
    write("LM --> "), writeln(LocalMem),
    any_list_concat(["Undefined variable ", VarName, "."], Message),
    writeln(Message).

report(env(GlobalMem, LocalMem, _, _), set, (VarName, _, _)) :-
    write("GM --> "), prettyVariables(GlobalMem, GM), writeln(GM),
    write("LM --> "), writeln(LocalMem),
    any_list_concat(["Variable ", VarName, " assigned value before defining."], Message),
    writeln(Message).

getVar(Env, Var) :-
    Env = env(GlobalMem, LocalMem, _, _),
    catch(getVar_(LocalMem, Var), undefined_variable, 
        catch(getVar_(GlobalMem, Var), undefined_variable, 
            report(Env, get, Var))),
    !.

% getVar(env(GlobalMem, _, _), Var) :-
%     getVar_(GlobalMem, Var),
%     !.

getVar_([], _) :-
    !,
    throw(undefined_variable).

getVar_([Var|_], Var) :- !.

getVar_([call_scope|_], (VarName, _)) :-
    !,
    any_list_concat(["Undefined variable ", VarName, " in current call scope."], Message),
    writeln(Message).

getVar_([_|Vars], Var) :-
    getVar_(Vars, Var).



setVar(Env, env(GM1, LM1, IG, EV), Var) :-
    Env = env(GM, LM, IG, EV),
    catch((setVar_(LM, LM1, Var), GM1 = GM), undefined_variable, 
        catch((setVar_(GM, GM1, Var), LM1 = LM), undefined_variable, 
            report(Env, set, Var))),
    default(GM1, GM),
    default(LM1, LM),
    !.

setVar_([], _, _) :-
    !,
    throw(undefined_variable).

setVar_([(VarName, _)|Mem], [(VarName, VarVal)|Mem], (VarName, VarVal)) :- !.

setVar_([call_scope|_], _, (VarName, _)) :-
    !,
    any_list_concat(["Undefined variable ", VarName, " in current call scope."], Message),
    writeln(Message).

setVar_([V|Vars], [V|Vars1], Var) :-
    setVar_(Vars, Vars1, Var).



createScope(env(GM, LocalMem, _, EV), env(GM, LocalMem1, false, EV)) :-
    LocalMem1 = [scope_boundary|LocalMem].

createNamedScope(env(GM, LocalMem, _, EV), env(GM, LocalMem1, false, EV), Name) :-
    LocalMem1 = [scope_boundary(Name)|LocalMem].

destroyScope(env(GM, LocalMem, _, EV), env(GM, LocalMem1, IsInGlobal, EV)) :-
    destroyScope_(LocalMem, LocalMem1),
    (LocalMem1 = [] -> IsInGlobal = true; IsInGlobal = false),
    !.

destroyScope_([], []) :- !.
destroyScope_([scope_boundary|Mem], Mem) :- !.
destroyScope_([scope_boundary(_)|Mem], Mem) :- !.
destroyScope_([call_scope|Mem], Mem) :-
    writeln("destroyScope_ tried to pass call scope!\nProbably bug in scope creation/destruction for something."),
    throw(call_scope_violation).
destroyScope_([_|Mem], Mem1) :-
    destroyScope_(Mem, Mem1).


createCall(env(GM, LocalMem, _, _), env(GM, LocalMem1, false, _), Args) :-
    append(Args, [call_scope|LocalMem], LocalMem1).

destroyCall(env(GM, LocalMem, _, _), env(GM, LocalMem1, IsInGlobal, _)) :-
    destroyCall_(LocalMem, LocalMem1),
    LocalMem1 = [] -> IsInGlobal = true; IsInGlobal = false,
    !.

destroyCall_([], []) :- !.
destroyCall_([call_scope|Mem], Mem) :- !.
destroyCall_([_|Mem], Mem1) :-
    destroyCall_(Mem, Mem1).

returnedValueDesc(EV, "not set") :- var(EV), !.
returnedValueDesc(EV, EV) :- nonvar(EV), !.


prettyEnv(env(GM, LM, IG, EV), Msg) :-
    prettyVariables(GM, PrettyGM),
    prettyVariables(LM, PrettyLM),
    returnedValueDesc(EV, EVDesc),
    any_list_concat(["[ENV]", " Globals: ", PrettyGM, " | Locals: ", PrettyLM, " | global: ", IG, ", returned: ", EVDesc], Msg).

prettyVariables(Variables, Friendly) :-
    maplist(friendlyVar, Variables, Friendly).

friendlyVar((VarName, closure(I, ArgNames, _)), (VarName, closure(I, ArgNames))) :- !.

friendlyVar((VarName, VarValue), (VarName, VarValue)) :- !.

friendlyVar(scope_boundary, sb).
friendlyVar(scope_boundary(Name), sb(Name)).
friendlyVar(call_scope, cs).