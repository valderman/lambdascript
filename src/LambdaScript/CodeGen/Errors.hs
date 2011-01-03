module LambdaScript.CodeGen.Errors where
import LambdaScript.CodeGen.Ops

lsError :: String -> Stmt
lsError msg = Return
            $ Call (Eval $ FunExp (FunIdent "error")) [Thunk $ Const $ strConst msg]

lambdaPatternMismatch =
  lsError "Pattern mismatch in lambda!"