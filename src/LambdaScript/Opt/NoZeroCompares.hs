-- | Doing foo == 0 is silly; better to replace it with !foo.
module LambdaScript.Opt.NoZeroCompares (noZeroCompares) where
import LambdaScript.CodeGen.Ops
import LambdaScript.Opt.Core

noZeroCompares :: Opt
noZeroCompares = Opt {
    optStm = id,
    optExp = noZero
  }

noZero :: Exp -> Exp
noZero (ConstrIs e 0)                   = Neg (Index e $ Const $ NumConst 0)
noZero (Oper Eq (Const (NumConst 0)) e) = Neg e
noZero (Oper Eq e (Const (NumConst 0))) = Neg e
noZero (Oper Ne (Const (NumConst 0)) e) = e
noZero (Oper Ne e (Const (NumConst 0))) = e
noZero x                                = x