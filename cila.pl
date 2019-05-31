:- ensure_loaded(lexer).
:- ensure_loaded(parser).
:- ensure_loaded(interp).
:- ensure_loaded(bytecoder).
:- ensure_loaded(compiler).
:- ensure_loaded(vm).

% "examples/recfib.cila"

main() :-
    current_prolog_flag(argv, [Action|_]),
    action(Action).


action('-i') :- 
    current_prolog_flag(argv, ['-i', File|_]),
    !,
    read_file_to_string(File, Str, []),
    lex(Str, Tokens), 
    parse(Tokens, Ast), 
    interp(Ast, _).

action('-c') :-
    current_prolog_flag(argv, ['-c', File|_]),
    !,
    read_file_to_string(File, Str, []),
    lex(Str, Tokens), 
    parse(Tokens, Ast), 
    bytecode_of(Ast, (Bytecode, Mappings)),
    compile((Bytecode, Mappings), Compiled),
    run(Compiled, (_, Memory)),
    applyVMVarNames(Memory, Mappings, Output),
    writeln(Output).

action(_) :-
    writeln("Unexpected option provided").