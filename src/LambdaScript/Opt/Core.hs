-- | Core framework for optimizing Lambdascript
module LambdaScript.Opt.Core (Opt (..), Optimize (..), newVars) where
import LambdaScript.CodeGen.Ops
import LambdaScript.CodeGen.Module

-- | An optimization. An optimization is just a pair of functions; one that
--   optimizes statements and one that optimizes expressions.
data Opt = Opt {
    optStm :: Stmt -> Stmt,
    optExp :: Exp -> Exp
  }

class Optimize a where
  optimize :: Opt -> a -> a

instance Optimize a => Optimize [a] where
  optimize o xs = map (optimize o) xs

instance Optimize Function where
  optimize = optF

instance Optimize Stmt where
  optimize = optS

instance Optimize Exp where
  optimize = optE

-- | Generate a list of new variables that are guaranteed not to name clash
--   with any pre-optimization variables. Note, however, that they may quite
--   possible clash with other vars taken from this list if care is not taken.
newVars :: [Var]
newVars = map (\n -> NamedTemp $ "_" ++ show n ++ "_") [0..]

-- | Optimize a function using the given optimization. As with 'optS',
--   optimization is bottom up.
optF :: Opt -> Function -> Function
optF o (Function n as ss t) =
  Function n as (map (optS o) ss) t


-- | Apply the given optimization recursively to the given statement.
--   The optimization is performed bottom up; that is, optimization is
--   performed on child nodes (then and else branches, for example) before
--   root nodes (the entire if statement.)
optS :: Opt -> Stmt -> Stmt
optS o (Assign v e) =
  optStm o $ Assign v (optE o e)
  
optS o (If e s ms) =
  optStm o $ If (optE o e) (optS o s) ms'
  where
    ms' = case ms of
      Just s -> Just $ optS o s
      _      -> Nothing
      
optS o (Return n e) =
  optStm o $ Return n (optE o e)
  
optS o (Block ss) =
  optStm o $ Block (map (optS o) ss)

optS o (Forever s) =
  optStm o $ Forever (optS o s)

optS o Break =
  optStm o Break

optS o (ExpStmt ex) =
  optStm o (ExpStmt $ optE o ex)

optS o NoStmt =
  NoStmt

optS o x =
  error $ "Can't optimize statement:\n" ++ show x


-- | Optimize an expression using the given optimization. Just like 'optE',
--   optimization is bottom up.
optE :: Opt -> Exp -> Exp
optE o (Thunk e) =
  optExp o $ Thunk (optE o e)

optE o (Eval e) =
  optExp o $ Eval (optE o e)

optE o (Tailcall e) =
  optExp o $ Tailcall (optE o e)

optE o (Index e1 e2) =
  optExp o $ Index (optE o e1) (optE o e2)

optE o (Array es) =
  optExp o $ Array (map (optE o) es)

optE o (ConstrIs e id v) =
  optExp o $ ConstrIs (optE o e) id v

optE o (Cons x xs) =
  optExp o $ Cons (optE o x) (optE o xs)

optE o e@(Const _) =
  optExp o e

optE o e@(Ident _) =
  optExp o e

optE o (Oper op e1 e2) =
  optExp o $ Oper op (optE o e1) (optE o e2)

optE o (Neg e) =
  optExp o $ Neg (optE o e)

optE o (Call n e es) =
  optExp o $ Call n (optE o e) (map (optE o) es)

optE o (FunExp (Lambda vs s)) =
  optExp o $ FunExp (Lambda vs (optS o s))

optE o e@(FunExp _) =
  optExp o e

optE o x =
  error $ "Can't optimize expression:\n" ++ show x
