-- | Remove any code that's unreachable because it comes after a return statement.
module LambdaScript.Opt.RemoveDeadCode (removeDeadCode) where
import LambdaScript.Opt.Core
import LambdaScript.CodeGen.Ops

removeDeadCode :: Opt
removeDeadCode = Opt {
    optStm = removeDead,
    optExp = id
  }

removeDead :: Stmt -> Stmt
removeDead b@(Block ss) =
  case span (not . isReturn) ss of
    (before, (return : _)) -> Block $ before ++ [return]
    _                      -> b
  where
    isReturn (Return _ _) = True
    isReturn _            = False
removeDead x = x
