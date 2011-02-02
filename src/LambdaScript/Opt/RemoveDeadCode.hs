-- | Remove any code that's unreachable because it comes after an unconditional
--   break.
module LambdaScript.Opt.RemoveDeadCode (removeDeadCode) where
import LambdaScript.Opt.Core
import LambdaScript.CodeGen.Ops

removeDeadCode :: Opt
removeDeadCode = Opt {
    optStm = removeDead,
    optExp = id
  }

removeDead :: Stmt -> Stmt
removeDead b@(Forever (Block ss)) =
  case span (not . isBreak) ss of
    (before, (break:_)) -> Forever $ Block $ before ++ [break]
    _                   -> b
  where
    isBreak Break = True
    isBreak _     = False
removeDead x = x
