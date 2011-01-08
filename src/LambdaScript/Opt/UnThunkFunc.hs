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
-- If it's a nullary function however, thunking it is still beneficial.
unThunkFunc :: Function -> Function
unThunkFunc (Function n m _ [Return arity (FunExp (Lambda as (Block b)))]) =
    Function n m as b
unThunkFunc (Function n m _ b) =
    Function n m [] [(SelfThunk n assignified)]
  where
    replaceLast [_] x'    = [x']
    replaceLast (x:xs) x' = x : replaceLast xs x'
    replaceLast _ x'      = [x']

    assignified =
      case last b of
        Return _ ex ->
          replaceLast b (Assign (Global $ n ++ ".x") ex)
        _ ->
          error $ "Last statement in lambda not return!\n" ++ show b
