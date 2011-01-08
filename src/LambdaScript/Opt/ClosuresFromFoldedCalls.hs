-- | Take a look at all Call expressions and turn those with arity >0 into
--   closures. This optimization must be run after FoldCalls, as it depends
--   on \a -> \b -> ... expressions being folded up into \a b -> ..., with
--   the arity giving the number of arguments missing for a complete
--   function application.
--   Apart from FoldCalls, this optimization also requires Uncurry to be active
--   in order to produce correct code.
module LambdaScript.Opt.ClosuresFromFoldedCalls (closuresFromFolded) where
import LambdaScript.Opt.Core
import LambdaScript.CodeGen.Ops

closuresFromFolded :: Opt
closuresFromFolded = Opt {
    optStm = id,
    optExp = closures
  }

-- | Fold a f(a)(b)(c) call into f(a,b,c).
--   Also update the function's arity, so a complementary optimization can turn
--   all function call expressions with arity >0 into closures.
closures :: Exp -> Exp
closures (Call n f args) | n > 0 =
  FunExp $ Lambda vars $ Block [Return 0 $ Call n f (args ++ map Ident vars)]
  where vars = take n newVars
closures exp =
  exp
