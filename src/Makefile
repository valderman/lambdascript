HSFILES=LambdaScript/TypeChecker.hs LambdaScript/TCM.hs Main.hs

all: .compiler

.compiler: $(HSFILES)
	ghc --make $(HSFLAGS) Main.hs
	touch .compiler

.bnfc: LambdaScript.cf
	bnfc -haskell -d LambdaScript LambdaScript.cf
	make -C LambdaScript .parser
	make -C LambdaScript .lexer
	touch .bnfc

clean:
	find . -name '*.hi' -exec rm \{\} \;
	find . -name '*.o' -exec rm \{\} \;
	rm .compiler .bnfc
	make -C LambdaScript clean
