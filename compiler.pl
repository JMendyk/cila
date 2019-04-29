
encode_int(X, [B1, B2, B3, B4]) :-
    integer(X), !,
    B4 is X mod (1 << 8),
    B3 is (X >> 8) mod (1 << 8),
    B2 is (X >> 16) mod (1 << 8),
    B1 is X >> 24.

encode_int(X, [B1, B2, B3, B4]) :-
    integer(B1), integer(B2), integer(B3), integer(B4),
    X is (B1 << 24) + (B2 << 16) + (B3 << 8) + B4.


get_head(T, T, T).

find(Label, _, LabelOffset) :-
    nonvar(LabelOffset),
    !,
    Label = LabelOffset.

find(Label, [(Label, Val)|_], Val) :- !.
find(Label, [_|Tail], Val) :-
    find(Label, Tail, Val).

put(Key, Val, [(Key, Val)|_]) :- !.
put(Key, Val, [(Other, _)|Tail]) :-
    Key \= Other,
    put(Key, Val, Tail).

:- op(1200, xfx, --->).

extended_pos(Pos0, Pos) :-
    '$expand':extended_pos(Pos0, 2, Pos).
term_expansion(
        compile(Cons, Asm, Arity) ---> Body,
        L,
        (compile(Offset, Lookup, Cons) --> (advance(Offset, Lookup, Arity, Offset1), NewBody)), L0) :-
        cbody((compile(Offset1, Lookup, Asm)), Body, NewBody),
        extended_pos(L, L0).
cbody(Extra, (B, Bs), (B, Rest)) :- !, cbody(Extra, Bs, Rest).
cbody(Extra, B, (B, Extra)).

advance(Offset, Lookup, Adv, Offset1, T, T) :-
    atom(Lookup), !,
    Offset1 is Offset + Adv.

advance(Offset, Lookup, Adv, Offset1, T, T) :-
    put(T, Offset, Lookup),
    Offset1 is Offset + Adv.

% load_int(X) - load X on stack
compile([0x1, X|Asm], Asm, 5) ---> [load_int(X)], !.
% compile(Offset, Lookup, [0x1, X|Asm]) --> advance(Offset, Lookup, 5, Offset1), [load_int(X)], !, compile(Offset1, Lookup, Asm).
% load(X) - load variable X's value on stack
compile([0x2, X|Asm], Asm, 5) ---> [load(X)], !.
% store(X) - pop value from stack and store it in variable X
compile([0x3, X|Asm], Asm, 5) ---> [store(X)], !.
% add, sub, mult, div, mod - pop top values from stack, perform proper operation and put result on stack
compile([0x4|Asm], Asm, 1) ---> [add], !.
compile([0x5|Asm], Asm, 1) ---> [sub], !.
compile([0x6|Asm], Asm, 1) ---> [mult], !.
compile([0x7|Asm], Asm, 1) ---> [div], !.
compile([0x8|Asm], Asm, 1) ---> [mod], !.
% and, or - pop two values from stack, perform proper operation and put result on stack
compile([0x9|Asm], Asm, 1) ---> [and], !.
compile([0xA|Asm], Asm, 1) ---> [or], !.
% not - pop top value from stack and put negated on stack
compile([0xB|Asm], Asm, 1) ---> [not], !.
% jump(label) - jump to given label
compile(Offset, Lookup, [0x10, LabelOffset|Asm]) --> advance(Offset, Lookup, 5, Offset1), [jump(Label)], !,
    { find(Label, Lookup, LabelOffset) }, 
    compile(Offset1, Lookup, Asm).
% jz(label) - pop value from stack and if it was zero jump to given label
compile(Offset, Lookup, [0x11, LabelOffset|Asm]) --> advance(Offset, Lookup, 5, Offset1), [jz(Label)], !,
    { find(Label, Lookup, LabelOffset) }, 
    compile(Offset1, Lookup, Asm).
% jgtz(label) - pop value from stack and if it was greater or equal zero jump to given label
compile(Offset, Lookup, [0x12, LabelOffset|Asm]) --> advance(Offset, Lookup, 5, Offset1), [jgtz(Label)], !,
    { find(Label, Lookup, LabelOffset) }, 
    compile(Offset1, Lookup, Asm).
% equal - pop two values from stack and push 1 if values were equal (otherwise 0)
compile([0xC|Asm], Asm, 1) ---> [equal], !.
% lt - pop two values from stack and push 1 if top was less than second (otherwise 0)
compile([0xD|Asm], Asm, 1) ---> [lt], !.
% leq - pop two values from stack and push 1 if top was less than or eqaul to second (otherwise 0)
compile([0xE|Asm], Asm, 1) ---> [leq], !.

compile(Offset, Lookup, []) --> advance(Offset, Lookup, 0, Offset), [].

expand([0x1, B1, B2, B3, B4|Asm]) --> [0x1, X], !, { encode_int(X, [B1, B2, B3, B4]) }, expand(Asm).
expand([0x2, B1, B2, B3, B4|Asm]) --> [0x2, X], !, { encode_int(X, [B1, B2, B3, B4]) }, expand(Asm).
expand([0x3, B1, B2, B3, B4|Asm]) --> [0x3, X], !, { encode_int(X, [B1, B2, B3, B4]) }, expand(Asm).
% expand([0x4|Asm]) --> [0x4], !, expand(Asm).
% expand([0x5|Asm]) --> [0x5], !, expand(Asm).
% expand([0x6|Asm]) --> [0x6], !, expand(Asm).
% expand([0x7|Asm]) --> [0x7], !, expand(Asm).
% expand([0x8|Asm]) --> [0x8], !, expand(Asm).
% expand([0x9|Asm]) --> [0x9], !, expand(Asm).
% expand([0xA|Asm]) --> [0xA], !, expand(Asm).
% expand([0xB|Asm]) --> [0xB], !, expand(Asm).
expand([0x10, B1, B2, B3, B4|Asm]) --> [0x10, LabelOffset], !, { encode_int(LabelOffset, [B1, B2, B3, B4]) }, expand(Asm).
expand([0x11, B1, B2, B3, B4|Asm]) --> [0x11, LabelOffset], !, { encode_int(LabelOffset, [B1, B2, B3, B4]) }, expand(Asm).
expand([0x12, B1, B2, B3, B4|Asm]) --> [0x12, LabelOffset], !, { encode_int(LabelOffset, [B1, B2, B3, B4]) }, expand(Asm).
% expand([0xC|Asm]) --> [0xC], !, expand(Asm).
% expand([0xD|Asm]) --> [0xD], !, expand(Asm).
% expand([0xE|Asm]) --> [0xE], !, expand(Asm).
expand([X|Asm]) --> [X], !, expand(Asm).

expand([]) --> [].

compile((Bytecode, _), Compiled) :-
    phrase(compile(0, _, CompBytecode), Bytecode),
    phrase(expand(Expanded), CompBytecode),
    Compiled = Expanded.

decompile(Compiled, Bytecode) :-
    Expanded = Compiled,
    phrase(expand(Expanded), CompBytecode),
    phrase(compile(0, lookup, CompBytecode), Bytecode).