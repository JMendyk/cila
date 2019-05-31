.PHONY: build submit

build: lexer.pl parser.pl interp.pl helpers.pl env_helpers.pl cila.pl
	mkdir -p build
	swipl -o build/cila -g main -t halt -c cila.pl

submit: 
	mkdir submit
	-cp -r * submit
	-cp -r .gitignore submit
	mv submit jakub_mendyk
	rm -r jakub_mendyk/submit
	tar -c jakub_mendyk -f jakub_mendyk.tar
	bzip2 jakub_mendyk.tar
	rm -r jakub_mendyk
