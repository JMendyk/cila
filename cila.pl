:- ensure_loaded(lexer).
:- ensure_loaded(parser).
:- ensure_loaded(interp).

% "examples/recfib.cila"

main():-
    current_prolog_flag(argv, [File|_]),
    read_file_to_string(File, Str, []),
    lex(Str, Tokens), 
    parse(Tokens, Ast), 
    interp(Ast, Env),
    prettyEnv(Env, PEnv).