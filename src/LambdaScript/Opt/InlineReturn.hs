-- | Replace all calls to io.return(thunk x) with thunk x.
module LambdaScript.Opt.InlineReturn (inlineReturn) where
import LambdaScript.CodeGen.Ops
import LambdaScript.Opt.Core

inlineReturn :: Opt
inlineReturn = Opt {
    optStm = id,
    optExp = inline
  }

inline :: Exp -> Exp
inline (Index (Call _ (Ident (Import "io" "return")) [arg]) (Const (NumConst 1))) =
  arg
inline x =
  x