module LambdaScript.CodeGen.Errors where
import LambdaScript.CodeGen.Ops

lsError :: String -> Stmt
lsError msg = Return
            $ Call (FunExp (FunIdent "error")) [Const $ strConst msg]

lambdaPatternMismatch =
  lsError "Pattern mismatch in lambda!"