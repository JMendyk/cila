.PHONY: build submit

build: repl.pl lexer.pl parser.pl interp.pl helpers.pl env_helpers.pl cila.pl
	mkdir -p build
	# swipl -o build/repl -g main -t halt -c repl.pl
	swipl -o build/cila -g main -t halt -c cila.pl

# repl: build
# 	./build/repl

submit: 
	mkdir submit
	-cp -r * submit
	-cp -r .gitignore .git submit
	mv submit jakub_mendyk
	rm -r jakub_mendyk/submit
	tar -c jakub_mendyk -f jakub_mendyk.tar
	bzip2 jakub_mendyk.tar
	rm -r jakub_mendyk
