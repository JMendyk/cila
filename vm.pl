:- consult(compiler).

simplify(X, Y) :-
    compound(X), !,
    term_to_atom(X, Y).

simplify([], "[]") :- !.

simplify(X, X).

lookup(_, [], 0) :- !.
lookup(X, [(X, V)|_], V) :- !.
lookup(X, [_|M], V) :-
    lookup(X, M, V).

assign(K, V, [], [(K, V)]) :- !.
assign(K, V, [(K, _)|M], [(K, V)|M]) :- !.
assign(K, V, [KV|M], [KV|M1]) :-
    assign(K, V, M, M1).

get_head(T, T, T).

% encode_int(X, [B1, B2, B3, B4]) :-
%     integer(X), !,
%     B4 is X mod (1 << 8),
%     B3 is (X >> 8) mod (1 << 8),
%     B2 is (X >> 16) mod (1 << 8),
%     B1 is X >> 24.

% encode_int(X, [B1, B2, B3, B4]) :-
%     integer(B1), integer(B2), integer(B3), integer(B4),
%     X is (B1 << 24) + (B2 << 16) + (B3 << 8) + B4.

decomp(0x1, load_int) :- !.
decomp(0x2, load) :- !.
decomp(0x3, store) :- !.
decomp(0x4, add) :- !.
decomp(0x5, sub) :- !.
decomp(0x6, mult) :- !.
decomp(0x7, div) :- !.
decomp(0x8, mod) :- !.
decomp(0x9, and) :- !.
decomp(0xA, or) :- !.
decomp(0xB, not) :- !.
decomp(0x10, jump) :- !.
decomp(0x11, jz) :- !.
decomp(0x12, jgtz) :- !.
decomp(0xC, equal) :- !.
decomp(0xD, lt) :- !.
decomp(0xE, leq) :- !.
decomp(X, X).

% vm(_, (S, M), _) --> get_head(H), { decompile(H, [Val|_]), maplist(simplify, [Val, S, M], Text), format('~w ~21| ~20w ~60| ~w\n', Text), fail }.

% load_int(X) 0x1    load(X) 0x2    store(X) 0x3    add 0x4    sub 0x5    mult 0x6    div 0x7    mod 0x8    and 0x9    or 0xA    not 0xB    jump(LabelOffset) 0x10    jz(LabelOffset) 0x11    jgtz(LabelOffset) 0x12    equal 0xC    lt 0xDleq 0xE

:- dynamic(jump_table/1).

% load_int(X) - load X on stack
vm((S, M), Out) --> [0x1, B1, B2, B3, B4], { encode_int(X, [B1, B2, B3, B4]) }, !, vm(([X|S], M), Out).
% load(X) - load variable X's value on stack
vm((S, M), Out) --> [0x2, B1, B2, B3, B4], { encode_int(X, [B1, B2, B3, B4]) }, !, { lookup(X, M, V) }, vm(([V|S], M), Out).
% store(X) - pop value from stack and store it in variable X
vm(([V|S], M), Out) --> [0x3, B1, B2, B3, B4], { encode_int(X, [B1, B2, B3, B4]) }, !, { assign(X, V, M, M1) }, vm((S, M1), Out).
% add, sub, mult, div, mod - pop top values from stack, perform proper operation and put result on stack
vm(([Arg1, Arg2|S], M), Out) --> [0x4], !, { Ret is Arg1 + Arg2 }, vm(([Ret|S], M), Out).
vm(([Arg1, Arg2|S], M), Out) --> [0x5], !, { Ret is Arg1 - Arg2 }, vm(([Ret|S], M), Out).
vm(([Arg1, Arg2|S], M), Out) --> [0x6], !, { Ret is Arg1 * Arg2 }, vm(([Ret|S], M), Out).
vm(([Arg1, Arg2|S], M), Out) --> [0x7], !, { Ret is Arg1 div Arg2 }, vm(([Ret|S], M), Out).
vm(([Arg1, Arg2|S], M), Out) --> [0x8], !, { Ret is Arg1 mod Arg2 }, vm(([Ret|S], M), Out).
% and, or - pop two values from stack, perform proper operation and put result on stack
vm(([1, 1|S], M), Out) --> [0x9], !, vm(([1|S], M), Out).
vm(([_, _|S], M), Out) --> [0x9], !, vm(([0|S], M), Out).
vm(([0, 0|S], M), Out) --> [0xA], !, vm(([0|S], M), Out).
vm(([_, _|S], M), Out) --> [0xA], !, vm(([1|S], M), Out).
% not - pop top value from stack and put negated on stack
vm(([1|S], M), Out) --> [0xB], !, vm(([0|S], M), Out).
vm(([0|S], M), Out) --> [0xB], !, vm(([1|S], M), Out).
% jump(label) - jump to given label
vm(In, Out) --> [0x10, B1, B2, B3, B4], { encode_int(Offset, [B1, B2, B3, B4]) }, !, { jump_table(Inits), arg(Offset, Inits, Bytecode), vm(In, Out, Bytecode, _) }.
% jz(label) - pop value from stack and if it was zero jump to given label
vm(([0|S], M), Out) --> [0x11, B1, B2, B3, B4], { encode_int(Offset, [B1, B2, B3, B4]) }, !, { jump_table(Inits), arg(Offset, Inits, Bytecode), vm((S, M), Out, Bytecode, _) }.
vm(([_|S], M), Out) --> [0x11, B1, B2, B3, B4], { encode_int(_, [B1, B2, B3, B4]) }, !, vm((S, M), Out).
% jgtz(label) - pop value from stack and if it was greater or equal zero jump to given label
vm(([V|S], M), Out) --> [0x12, B1, B2, B3, B4], { encode_int(Offset, [B1, B2, B3, B4]) }, { V >= 0}, !, { jump_table(Inits), arg(Offset, Inits, Bytecode), vm((S, M), Out, Bytecode, _) }.
vm(([_|S], M), Out) --> [0x12, B1, B2, B3, B4], { encode_int(_, [B1, B2, B3, B4]) }, !, vm((S, M), Out).
% equal - pop two values from stack and push 1 if values were equal (otherwise 0)
vm(([X, X|S], M), Out) --> [0xC], !, vm(([1|S], M), Out).
vm(([_, _|S], M), Out) --> [0xC], !, vm(([0|S], M), Out).
% lt - pop two values from stack and push 1 if top was less than second (otherwise 0)
vm(([X, Y|S], M), Out) --> [0xD], { X < Y }, !, vm(([1|S], M), Out).
vm(([_, _|S], M), Out) --> [0xD], !, vm(([0|S], M), Out).
% leq - pop two values from stack and push 1 if top was less than or eqaul to second (otherwise 0)
vm(([X, Y|S], M), Out) --> [0xE], { X =< Y }, !, vm(([1|S], M), Out).
vm(([_, _|S], M), Out) --> [0xE], !, vm(([0|S], M), Out).

vm(Memory, Memory) --> [].

% prefixes([X|Xs], [[X|Xs]|Prefixes]) :-
%     prefixes(Xs, Prefixes).
% prefixes([], [[]]).

make_jump_table(Xs, JumpMapping, Inits) :-
    make_jump_table(0, Xs, JumpMapping, Inits).

make_jump_table(N, [X|Xs], [N|JM], [[X|Xs]|Inits]) :-
    !,
    N1 is N + 1,
    make_jump_table(N1, Xs, JM, Inits).

make_jump_table(N, [_|Xs], JM, Inits) :-
    !,
    N1 is N + 1,
    make_jump_table(N1, Xs, JM, Inits).

make_jump_table(N, [], [N], [[]]) :- !.

make_jump_table(_, _, [], []).


run((Bytecode, JumpMapping), (S, M)) :-
    make_jump_table(Bytecode, JumpMapping, Inits),
    Faster =.. [array|Inits],
    asserta(jump_table(Faster)),
    !,
    phrase(vm(([], []), (S, M)), Bytecode, _),
    retract(jump_table(Faster)).

applyVMVarNames([], [], []).

applyVMVarNames([(Idx, Value)|Idxs], [Name|Names], [(Name, Value)|Tail]) :-
    applyVMVarNames(Idxs, Names, Tail).