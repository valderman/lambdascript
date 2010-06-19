HSFILES=

all: .compiler

.compiler: LambdaScript.cf $(HSFILES)
	bnfc -haskell -d LambdaScript LambdaScript.cf
	make -C LambdaScript .parser
	make -C LambdaScript .lexer
	touch .compiler

clean:
	find . -name '*.hi' -exec rm \{\} \;
	find . -name '*.o' -exec rm \{\} \;
	rm .compiler
	make -C LambdaScript clean
