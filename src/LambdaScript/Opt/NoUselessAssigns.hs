-- | Where an assignment like var1 = var2 is made and it's not inside an if or
--   loop, delete the assignment and instead replace every occurrence of var1
--   with var2 in the rest of the function.
module LambdaScript.Opt.NoUselessAssigns (noUselessAssigns) where
import LambdaScript.Opt.Core
import LambdaScript.CodeGen.Ops
import Data.List (foldl')
import Data.Maybe (catMaybes, listToMaybe)

noUselessAssigns :: Opt
noUselessAssigns = Opt {
    optStm = id,
    optExp = removeAss
  }

findAs :: Stmt -> Maybe (Var, Var)
findAs (Block stmts)         = listToMaybe . catMaybes $ map findAs stmts
findAs (Assign v (Ident v')) = Just (v, v')
findAs (SelfThunk _ stmts)   = listToMaybe . catMaybes $ map findAs stmts
findAs (If _ th elm)         = case (findAs th, elm) of
                                 (Nothing, Just el) -> findAs el
                                 (th', _)           -> th'
findAs (Return _ exp)        = findAsEx exp
findAs _                     = Nothing

findAsEx :: Exp -> Maybe (Var, Var)
findAsEx (FunExp (Lambda _ s)) = findAs s
findAsEx _                     = Nothing

removeAss :: Exp -> Exp
removeAss ex@(FunExp (Lambda vs b)) =
  case findAs b of
    Just (v, v') ->
      removeAss $ FunExp (Lambda vs $ killAssign v v' $ optimize (subst v v') b)
    _ ->
      ex
removeAss x =
  x

-- Apply the substitution as an optimization, since we can then take advantage
-- of the common traversing infrastructure.
subst :: Var -> Var -> Opt
subst v v' = Opt {
    optStm = killAssign v v',
    optExp = substVar v v'
  }

killAssign :: Var -> Var -> Stmt -> Stmt
killAssign v v' (Assign x (Ident x'))
  | x == v && x' == v' =
    NoStmt
killAssign _ _ x =
  x


substVar :: Var -> Var -> Exp -> Exp
substVar v new (Ident v') | (v == v') =
  Ident new
substVar _ _ x =
  x
