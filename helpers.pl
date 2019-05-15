
zip([], [], []) :- !.
zip([X|Xs], [Y|Ys], [(X, Y)|XYs]) :-
    zip(Xs, Ys, XYs).

any_list_concat(Any, Concat) :-
    any_list_concat_(Any, Atoms),
    atomic_list_concat(Atoms, Concat).

any_list_concat(Any, Sep, Concat) :-
    any_list_concat_(Any, Atoms),
    atomic_list_concat(Atoms, Sep, Concat).

any_list_concat_([], []) :- !.
any_list_concat_([[]|Xs], [Y|Ys]) :-
    !,
    term_to_atom([], Y),
    any_list_concat_(Xs, Ys).
any_list_concat_([X|Xs], [X|Ys]) :-
    atomic(X),
    !,
    any_list_concat_(Xs, Ys).
any_list_concat_([X|Xs], [Y|Ys]) :-
    compound(X),
    !,
    term_to_atom(X, Y),
    any_list_concat_(Xs, Ys).

% Array representation helpers:

arr_set(array(Length, _), Idx, _, _) :-
    Idx >= Length,
    throw("List too short!").

arr_set(array(Length, Values), Idx, Val, array(Length, Result)) :-
    arr_set_(Values, Idx, Val, Result).

arr_set_([_|Vs], 0, V, [V|Vs]) :- !.
arr_set_([V1|Vs], N, V, [V1|Vs1]) :-
    N1 is N - 1,
    arr_set_(Vs, N1, V, Vs1).



arr_get(array(Length, _), Idx, _) :-
    Idx >= Length,
    throw("List too short!").

arr_get(array(_, Values), Idx, Result) :-
    arr_get_(Values, Idx, Result).

arr_get_([V|_], 0, V) :- !.
arr_get_([_|Vs], N, V) :-
    N1 is N - 1,
    arr_get_(Vs, N1, V).

% repeatingList(list with ? elements, +)
% predicate for lists that are a finite prefix of infinite concatenation of second list

repeatingList(Ls, Ts) :-
    repeatingList(Ls, Ts, Ts).

repeatingList([T|Ls], [T|Ts], FullTs) :-
    !,
    repeatingList(Ls, Ts, FullTs).

repeatingList(Ls, [], FullTs) :-
    !,
    repeatingList(Ls, FullTs, FullTs).

repeatingList([], _, _).

% Evaluation helpers

parallelEval([], _, _, []).

parallelEval([E|Es], EvalPred, Mem, [V|Vs]) :-
    call(EvalPred, E, Mem, V),
    parallelEval(Es, EvalPred, Mem, Vs).

sequentialEval([], _, Env, Env) :- !.
sequentialEval([S|Ss], EvalPred, Env, EnvOut) :-
    call(EvalPred, S, Env, Env1),
    sequentialEval(Ss, EvalPred, Env1, EnvOut).

% Evaluate Second argument if first is true, otherwise third argument
evalBranch(true, EvalPred, Then, _, Mem, MemOut) :-
    !,
    call(EvalPred, Then, Mem, MemOut).

evalBranch(false, EvalPred, _, Else, Mem, MemOut) :-
    !,
    call(EvalPred, Else, Mem, MemOut).

selectBranch(true, Then, _, Mem, MemOut) :- 
    !,
    call(Then, Mem, MemOut).

selectBranch(false, _, Else, Mem, MemOut) :-
    !,
    call(Else, Mem, MemOut).