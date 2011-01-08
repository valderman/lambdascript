module LambdaScript.CodeGen.Errors where
import LambdaScript.CodeGen.Ops

lsError :: String -> Stmt
lsError msg = Return 0
            $ Call 0 (Eval $ FunExp (FunIdent "error")) [Thunk $ Const $ strConst msg]

lambdaPatternMismatch =
  lsError "Pattern mismatch in lambda!"