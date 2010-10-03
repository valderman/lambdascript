HSFILES=LambdaScript/TypeChecker.hs LambdaScript/TCM.hs Main.hs LambdaScript/Depends.hs LambdaScript/Desugar.hs LambdaScript/Types.hs LambdaScript/CodeGen/Monad.hs LambdaScript/CodeGen/Ops.hs LambdaScript/CodeGen/Module.hs

all: .compiler

.compiler: .bnfc $(HSFILES)
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
