-- | Doing [x, y][1] is stupid, so we replace it with simply y.
module LambdaScript.Opt.ZapArrays (zapArrays) where
import LambdaScript.Opt.Core
import LambdaScript.CodeGen.Ops

zapArrays :: Opt
zapArrays = Opt {
    optStm = id,
    optExp = zap
  }

zap :: Exp -> Exp
zap (Index (Array exs) (Const (NumConst ix))) =
  exs !! truncate ix
zap x =
  x
