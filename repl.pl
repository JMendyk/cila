library(readline).
:- consult(interp).

main() :-
    writeln("CILA repl v0"),
    emptyInterpMemory(Memory),
    loop(Memory).

loop(InMemory) :-
    write("> "),
    read_line_to_string(user_input, Str),
    rl_add_history(Str),
    interpWithMemory(Str, InMemory, OutMemory),
    writeln(OutMemory),
    loop(OutMemory).