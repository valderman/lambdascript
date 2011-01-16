-- | Replace all calls to io.return(thunk x) with thunk x.
module LambdaScript.Opt.InlineReturn (inlineReturn) where
import LambdaScript.CodeGen.Ops
import LambdaScript.Opt.Core
import LambdaScript.Types

inlineReturn :: Opt
inlineReturn = Opt {
    optStm = id,
    optExp = inline
  }

inline :: Exp -> Exp
inline (Call n (Ident (Import "io" "return")) [arg]) =
  Call n (FunExp $ Construct ((tv "a") ~> io (tv "a")) (-1)) [arg]
inline x =
  x