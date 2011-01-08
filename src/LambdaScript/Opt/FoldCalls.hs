-- | Turn all f(a)(b) calls into f(a,b).
--   Note that this optimization depends on the Uncurry and
--   ClosuresFromFoldedCalls optimizations to generate correct code.
module LambdaScript.Opt.FoldCalls (foldCalls) where
import LambdaScript.Opt.Core
import LambdaScript.CodeGen.Ops

foldCalls :: Opt
foldCalls = Opt {
    optStm = id,
    optExp = foldCall
  }

-- | Fold a f(a)(b)(c) call into f(a,b,c).
--   Also update the function's arity, so a complementary optimization can turn
--   all function call expressions with arity >0 into closures.
foldCall :: Exp -> Exp
foldCall (Call n (Call _ f args') args) =
  Call n f as
  where as = args' ++ args
foldCall exp =
  exp
