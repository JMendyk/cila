.PHONY: build repl submit

build: repl.pl interp.pl
	mkdir -p build
	swipl -o build/repl -g main -t halt -c repl.pl
	swipl -o build/cila -g main -t halt -c cila.pl

repl: build
	./build/repl

submit: 
	mkdir submit
	-cp -r * submit
	mv submit jakub_mendyk
	rm -r jakub_mendyk/submit
	tar -c jakub_mendyk -f jakub_mendyk.tar
	bzip2 jakub_mendyk.tar
	rm -r jakub_mendyk
