HSFILES=LambdaScript/TypeChecker.hs LambdaScript/TCM.hs Main.hs LambdaScript/Depends.hs LambdaScript/Desugar.hs LambdaScript/Types.hs LambdaScript/CodeGen/Monad.hs LambdaScript/CodeGen/Ops.hs LambdaScript/CodeGen/Module.hs LambdaScript/CodeGen/Errors.hs LambdaScript/CodeGen/Generate.hs LambdaScript/Builtins.hs LambdaScript/CodeGen/ShowJS.hs LambdaScript/Opt/Core.hs LambdaScript/Opt/BooleanSimplifier.hs LambdaScript/Opt/Optimize.hs LambdaScript/Opt/NoZeroCompares.hs LambdaScript/Opt/NoObviousIfs.hs LambdaScript/Opt/ReduceBlocks.hs LambdaScript/Opt/NoUselessAssigns.hs LambdaScript/Opt/ZapArrays.hs LambdaScript/Annotate.hs LambdaScript/Opt/Uncurry.hs LambdaScript/Opt/FoldCalls.hs LambdaScript/Opt/ClosuresFromFoldedCalls.hs LambdaScript/Opt/UnThunkFunc.hs LambdaScript/Make.hs Args.hs LambdaScript/CodeGen/GenTypes.hs LambdaScript/Opt/InlineReturn.hs LambdaScript/Opt/InlineJSFun.hs LambdaScript/Opt/RemoveDeadCode.hs LambdaScript/Opt/TCE.hs

all: .compiler

.compiler: .bnfc $(HSFILES)
	ghc --make $(HSFLAGS) -o ../lsc Main.hs
	touch .compiler

.bnfc: LambdaScript.cf
	bnfc -haskell -d LambdaScript LambdaScript.cf
	make -C LambdaScript .parser
	make -C LambdaScript .lexer
	touch .bnfc

doc:
	mkdir -p ../docs/haddock
	haddock --ignore-all-exports --odir=../docs/haddock -h Main.hs


clean:
	find . -name '*.hi' -exec rm \{\} \;
	find . -name '*.o' -exec rm \{\} \;
	rm .compiler .bnfc
	make -C LambdaScript clean
