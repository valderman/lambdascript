-- | Where an assignment like var1 = var2 is made and it's not inside an if or
--   loop, delete the assignment and instead replace every occurrence of var1
--   with var2 in the rest of the function.
module LambdaScript.Opt.NoUselessAssigns (noUselessAssigns) where
import LambdaScript.Opt.Core
import LambdaScript.CodeGen.Ops

noUselessAssigns :: Opt
noUselessAssigns = Opt {
    optStm = id,
    optExp = removeAss
  }

uselessAssign :: Stmt -> Bool
uselessAssign (Assign _ (Ident _)) = True
uselessAssign _                    = False

removeAss :: Exp -> Exp
removeAss ex@(FunExp (Lambda vs (Block ss))) =
  case span (not . uselessAssign) ss of
    (pre, []) ->
      ex
    (pre, (Assign v (Ident v')):post) ->
      removeAss $ FunExp (Lambda vs $ Block $ pre ++ optimize (subst v v') post)
    _ ->
      ex
removeAss x =
  x

-- Apply the substitution as an optimization, since we can then take advantage
-- of the common traversing infrastructure.
subst :: Var -> Var -> Opt
subst v v' = Opt {
    optStm = id,
    optExp = substVar v v'
  }

substVar :: Var -> Var -> Exp -> Exp
substVar v new (Ident v') | (v == v') =
  Ident new
substVar _ _ x =
  x