-- | Ensures that all globals are non-thunks.
--   Codegen expects this and relies on it to generate correct code, so it's
--   sort of important.
module LambdaScript.Opt.UnThunkFunc (unEvalGlobals, unThunkFunc) where
import LambdaScript.CodeGen.Ops
import LambdaScript.CodeGen.Module
import LambdaScript.Opt.Core
import LambdaScript.Abs (Type(..), TIdent(..))

unEvalGlobals :: Opt
unEvalGlobals = Opt {
    optStm = id,
    optExp = unEvalGlobal
  }

unEvalGlobal :: Exp -> Exp
unEvalGlobal (Call n (Eval f@(Ident (Global arity _))) args) | arity > 0 =
  Call n f (map thunkGlobal args)
unEvalGlobal (Call n (Eval f@(Ident (Import arity _ a))) args) | arity > 0 =
  Call n f (map thunkGlobal args)
unEvalGlobal (Call n (Eval f@(Ident (Builtin a))) args) =
  Call n f (map thunkGlobal args)
unEvalGlobal (Call n f args) =
  Call n f (map thunkGlobal args)
unEvalGlobal x =
  x

thunkGlobal g@(Ident (Global n _))   | n > 0 = Thunk g
thunkGlobal g@(Ident (Import n _ _)) | n > 0 = Thunk g
thunkGlobal x                              = x

-- | Having global functions as thunks isn't really beneficial in any way.
--   Since the body of every function only consists of a lambda function,
--   just shuffle the args around to get rid of one redirection.
--   If it's a nullary function however, thunking it is still beneficial.
unThunkFunc :: Function -> Function
unThunkFunc (Function n _ [Return arity (FunExp (Lambda as (Block b)))] t) =
  Function n as b t
-- Take the liberty of using a named temp variable "a" to transfer the
-- argument; no code runs between its creation and last use, so name capture
-- is impossible.
unThunkFunc (Function n [] [Return arity ex] t) | arity > 0 =
  Function n [NamedTemp "a"] [
      Return (arity-1) (Call arity (unEval ex) [Ident $ NamedTemp "a"])
    ] t
  where
    unEval (Eval ex) = ex
    unEval ex        = ex
unThunkFunc fun@(Function n _ b t) =
  case t of
    TApp (TCon (TIdent "IO")) _ ->
      fun
    _ ->
      Function n [] [(SelfThunk n assignified)] t
  where
    replaceLast [_] x'    = x'
    replaceLast (x:xs) x' = x : replaceLast xs x'
    replaceLast _ x'      = x'

    assignified =
      case last b of
        Return _ ex ->
          replaceLast b [Assign (Global 0 $ n ++ ".e") $ Const $ NumConst 1,
                         Assign (Global 0 $ n ++ ".x") ex]
        _ ->
          error $ "Last statement in lambda not return!\n" ++ show b
