-- | Remove all if-statements where the branch condition is trivially 1 or 0.
module LambdaScript.Opt.NoObviousIfs (noObviousIfs) where
import LambdaScript.Opt.Core
import LambdaScript.CodeGen.Ops

noObviousIfs :: Opt
noObviousIfs = Opt {
    optStm = removeIfs,
    optExp = id
  }

isOne :: Exp -> Bool
isOne (Const (BoolConst True))      = True
isOne (Const (NumConst n)) | n /= 0 = True
isOne _                             = False

isZero :: Exp -> Bool
isZero (Const (BoolConst False)) = True
isZero (Const (NumConst 0))      = True
isZero _                         = False

removeIfs :: Stmt -> Stmt
removeIfs (If cond thenS melseS)
  | isOne cond  = thenS
  | isZero cond = case melseS of
                    Just elseS -> elseS
                    _           -> NoStmt
removeIfs x = x