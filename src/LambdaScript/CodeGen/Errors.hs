module LambdaScript.CodeGen.Errors where
import LambdaScript.CodeGen.Ops

lsError msg = Call (FunExp (FunIdent "error")) [Const $ StrConst msg]

lambdaPatternMismatch =
  lsError "Pattern mismatch in lambda!"