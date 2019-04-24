.PHONY: build repl

build: repl.pl interp.pl
	mkdir -p build
	swipl -o build/repl -g main -t halt -c repl.pl

repl: build
	./build/repl	