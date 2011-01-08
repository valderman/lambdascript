-- | Ensures that all globals are non-thunks.
--   Codegen expects this and relies on it to generate correct code, so it's
--   sort of important.
module LambdaScript.Opt.UnThunkFunc (unEvalGlobals, unThunkFunc) where
import LambdaScript.CodeGen.Ops
import LambdaScript.CodeGen.Module
import LambdaScript.Opt.Core

unEvalGlobals :: Opt
unEvalGlobals = Opt {
    optStm = id,
    optExp = unEvalGlobal
  }

unEvalGlobal :: Exp -> Exp
unEvalGlobal (Call n (Eval f@(Ident (Global _))) args) =
  Call n f args
unEvalGlobal x =
  x

-- Having global functions as thunks isn't really beneficial in any way.
-- Since the body of every function only consists of a lambda function,
-- just shuffle the args around to get rid of one redirection.
unThunkFunc :: Function -> Function
unThunkFunc (Function n m _ [Return _ (FunExp (Lambda as (Block b)))]) =
  Function n m as b
unThunkFunc x =
  x