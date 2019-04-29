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

% load_int(X) - load X on stack
vm(Inits, (S, M), Out) --> [0x1, B1, B2, B3, B4], { encode_int(X, [B1, B2, B3, B4]) }, !, vm(Inits, ([X|S], M), Out).
% load(X) - load variable X's value on stack
vm(Inits, (S, M), Out) --> [0x2, B1, B2, B3, B4], { encode_int(X, [B1, B2, B3, B4]) }, !, { lookup(X, M, V) }, vm(Inits, ([V|S], M), Out).
% store(X) - pop value from stack and store it in variable X
vm(Inits, ([V|S], M), Out) --> [0x3, B1, B2, B3, B4], { encode_int(X, [B1, B2, B3, B4]) }, !, { assign(X, V, M, M1) }, vm(Inits, (S, M1), Out).
% add, sub, mult, div, mod - pop top values from stack, perform proper operation and put result on stack
vm(Inits, ([Arg1, Arg2|S], M), Out) --> [0x4], !, { Ret is Arg1 + Arg2 }, vm(Inits, ([Ret|S], M), Out).
vm(Inits, ([Arg1, Arg2|S], M), Out) --> [0x5], !, { Ret is Arg1 - Arg2 }, vm(Inits, ([Ret|S], M), Out).
vm(Inits, ([Arg1, Arg2|S], M), Out) --> [0x6], !, { Ret is Arg1 * Arg2 }, vm(Inits, ([Ret|S], M), Out).
vm(Inits, ([Arg1, Arg2|S], M), Out) --> [0x7], !, { Ret is Arg1 div Arg2 }, vm(Inits, ([Ret|S], M), Out).
vm(Inits, ([Arg1, Arg2|S], M), Out) --> [0x8], !, { Ret is Arg1 mod Arg2 }, vm(Inits, ([Ret|S], M), Out).
% and, or - pop two values from stack, perform proper operation and put result on stack
vm(Inits, ([1, 1|S], M), Out) --> [0x9], !, vm(Inits, ([1|S], M), Out).
vm(Inits, ([_, _|S], M), Out) --> [0x9], !, vm(Inits, ([0|S], M), Out).
vm(Inits, ([0, 0|S], M), Out) --> [0xA], !, vm(Inits, ([0|S], M), Out).
vm(Inits, ([_, _|S], M), Out) --> [0xA], !, vm(Inits, ([1|S], M), Out).
% not - pop top value from stack and put negated on stack
vm(Inits, ([1|S], M), Out) --> [0xB], !, vm(Inits, ([0|S], M), Out).
vm(Inits, ([0|S], M), Out) --> [0xB], !, vm(Inits, ([1|S], M), Out).
% jump(label) - jump to given label
vm(Inits, In, Out) --> [0x10, B1, B2, B3, B4], { encode_int(Offset, [B1, B2, B3, B4]) }, !, { Offset1 is Offset + 1, nth1(Offset1, Inits, Bytecode), phrase(vm(Inits, In, Out), Bytecode, _) }.
% jz(label) - pop value from stack and if it was zero jump to given label
vm(Inits, ([0|S], M), Out) --> [0x11, B1, B2, B3, B4], { encode_int(Offset, [B1, B2, B3, B4]) }, !, { Offset1 is Offset + 1, nth1(Offset1, Inits, Bytecode), phrase(vm(Inits, (S, M), Out), Bytecode, _) }.
vm(Inits, ([_|S], M), Out) --> [0x11, B1, B2, B3, B4], { encode_int(_, [B1, B2, B3, B4]) }, !, vm(Inits, (S, M), Out).
% jgtz(label) - pop value from stack and if it was greater or equal zero jump to given label
vm(Inits, ([V|S], M), Out) --> [0x12, B1, B2, B3, B4], { encode_int(Offset, [B1, B2, B3, B4]) }, { V >= 0}, !, { Offset1 is Offset + 1, nth1(Offset1, Inits, Bytecode), phrase(vm(Inits, (S, M), Out), Bytecode, _) }.
vm(Inits, ([_|S], M), Out) --> [0x12, B1, B2, B3, B4], { encode_int(_, [B1, B2, B3, B4]) }, !, vm(Inits, (S, M), Out).
% equal - pop two values from stack and push 1 if values were equal (otherwise 0)
vm(Inits, ([X, X|S], M), Out) --> [0xC], !, vm(Inits, ([1|S], M), Out).
vm(Inits, ([_, _|S], M), Out) --> [0xC], !, vm(Inits, ([0|S], M), Out).
% lt - pop two values from stack and push 1 if top was less than second (otherwise 0)
vm(Inits, ([X, Y|S], M), Out) --> [0xD], { X < Y }, !, vm(Inits, ([1|S], M), Out).
vm(Inits, ([_, _|S], M), Out) --> [0xD], !, vm(Inits, ([0|S], M), Out).
% leq - pop two values from stack and push 1 if top was less than or eqaul to second (otherwise 0)
vm(Inits, ([X, Y|S], M), Out) --> [0xE], { X =< Y }, !, vm(Inits, ([1|S], M), Out).
vm(Inits, ([_, _|S], M), Out) --> [0xE], !, vm(Inits, ([0|S], M), Out).

vm(_, Memory, Memory) --> [].

prefixes([X|Xs], [[X|Xs]|Prefixes]) :-
    prefixes(Xs, Prefixes).
prefixes([], [[]]).

run(Bytecode, (S, M)) :-
    prefixes(Bytecode, Inits),
    % writeln(Inits),
    phrase(vm(Inits, ([], []), (S, M)), Bytecode, _).
