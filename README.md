# CILA _(abbrev. CILA Isn't a Language Anyway)_
_programming language_ [github.com/JMendyk/cila](https://github.com/JMendyk/cila)

Written with � in Prolog as a final project for half-semester course at University of Wrocław.

Examples programs are provided in `examples` directory.

## Building

Build interpreter and bytecoder/vm using `make build`, file `build/cila` will be created.

## Using interpreter

Execute interpreter program using `./build/cila -i <filename>`.

## Using VM

Execute VM program using `./build/cila -c <filename>`.
Not all language features are supported in bytecode conversion process - after initial tests it came out that tree-walking interpreter has much better performance and bytecode conversion / VM idea has been abandoned. Arrays and functions (including read and print) are not supported by bytecode conversion process. Example program that can be executed in VM is provided in file `examples/compileable_isprime.cila`. Since program can't communicate with the world by itself, memory state at the end of the execution will be printed to the screen by VM.