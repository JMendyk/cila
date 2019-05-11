% Predicates for working with interpreter's environment

default(Var, _) :-
    nonvar(Var), !.
default(Var, Default) :-
    var(Var),
    Var = Default.

% Environment: env(GlobalMem, LocalMem, IsInGlobalScope)
initEnv(env([], [], true)).



isInGlobalScope(env(_, _, IsInGlobalScope), IsInGlobalScope).



createVar(env(GlobalMem, LM, true), env(GlobalMem1, LM, true), Var) :-
    createVar_(GlobalMem, GlobalMem1, Var),
    !.

createVar(env(GM, LocalMem, false), env(GM, LocalMem1, false), Var) :-
    createVar_(LocalMem, LocalMem1, Var).

createVar_(Mem, [(VarName, VarValue)|Mem], (VarName, VarValue)).

report(env(GlobalMem, LocalMem, _), Locality, (VarName, _)) :-
    writeln(GlobalMem),
    writeln(LocalMem),
    atomics_to_string(["Undefined", Locality,"variable", VarName], " ", Message),
    writeln(Message).

getVar(Env, Var) :-
    Env = env(GlobalMem, LocalMem, _),
    catch(getVar_(LocalMem, Var), undefined_variable, 
        catch(getVar_(GlobalMem, Var), undefined_variable, 
            report(Env, global, Var))),
    !.

% getVar(env(GlobalMem, _, _), Var) :-
%     getVar_(GlobalMem, Var),
%     !.

getVar_([], _) :-
    !,
    throw(undefined_variable).

getVar_([Var|_], Var) :- !.

getVar_([_|Vars], Var) :-
    getVar_(Vars, Var).



setVar(Env, env(GM1, LM1, IG), Var) :-
    Env = env(GM, LM, IG),
    catch((setVar_(LM, LM1, Var), GM1 = GM), undefined_variable, 
        catch((setVar_(GM, GM1, Var), LM1 = LM), undefined_variable, 
            report(Env, global, Var))),
    default(GM1, GM),
    default(LM1, LM),
    !.

setVar_([], _, _) :-
    !,
    throw(undefined_variable).

setVar_([(VarName, _)|Mem], [(VarName, VarVal)|Mem], (VarName, VarVal)) :- !.

setVar_([V|Vars], [V|Vars1], Var) :-
    setVar_(Vars, Vars1, Var).



createScope(env(GM, LocalMem, _), env(GM, LocalMem1, false)) :-
    LocalMem1 = [scope_boundary|LocalMem].



destroyScope(env(GM, LocalMem, _), env(GM, LocalMem1, IsInGlobal)) :-
    destroyScope_(LocalMem, LocalMem1),
    LocalMem1 = [] -> IsInGlobal = true; IsInGlobal = false,
    !.

destroyScope_([], []) :- !.
destroyScope_([scope_boundary|Mem], Mem) :- !.
destroyScope_([_|Mem], Mem1) :-
    destroyScope_(Mem, Mem1).

% withBlockScope(Goal) -->
%     insertBlockBound,
%     Goal,
%     clearToBlockBound.

% pure(Env, Env, Goal) :-
%     writeln(Goal),
%     Goal =.. [GHead|GTail],
%     PureGoal =.. [GHead, Env|GTail],
%     call(PureGoal).

% insertBlockBound(_, _).
% clearToBlockBound(_, _).

