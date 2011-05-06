-- | Replace all instances of Return $ Call ... by Return Tailcall
module LambdaScript.Opt.TCE (eliminateTailCalls) where
import Data.List (intersect, foldl')
import Data.Maybe (maybeToList)
import LambdaScript.CodeGen.Ops
import LambdaScript.CodeGen.Module
import LambdaScript.Opt.Core

uselessVars :: Function -> [Var]
uselessVars = uvF

uvS :: Stmt -> [Var]
uvS (Assign _ exp)              = uvE exp
uvS (AssignResult _ exp)        = uvE exp
-- Be very conservative about tail calls for a start;
-- let y = f x in if foo then y else something_else does NOT contain
-- a tail call!
uvS (If ex s1 s2)               = uvE ex ++ 
                                  (uvS s1 `intersect`
                                   (concat $ maybeToList $ fmap uvS s2))
uvS (Return n (Ident v))        = [v]
uvS (Return n (Eval (Ident v))) = [v]
uvS (Return n (StmtEx ss (Ident v))) =
  v:concat (map uvS ss)
uvS (Return n (StmtEx ss (Eval (Ident v)))) =
  v:concat (map uvS ss)
uvS (Return n x)                = uvE x
uvS (Block ss)                  = concat $ map uvS ss
uvS (ExpStmt ex)                = uvE ex
uvS (Forever s)                 = uvS s
uvS _                           = []

uvE :: Exp -> [Var]
uvE (Thunk ex) = uvE ex
uvE (IOThunk ex) = uvE ex
uvE (Eval ex) = uvE ex
uvE (StmtEx ss ex) = concat $ uvE ex : map uvS ss
uvE (Index e1 e2) = uvE e1 ++ uvE e2
uvE (Array exs) = concat $ map uvE exs
uvE (ConstrIs e _ _) = uvE e
uvE (Cons e1 e2) = uvE e1 ++ uvE e2
uvE (Oper _ e1 e2) = uvE e1 ++ uvE e2
uvE (Neg e) = uvE e
uvE (Call _ e es) = concat $ map uvE (e:es)
uvE (FunExp (Lambda _ s)) = uvS s
uvE _ = []

uvF :: Function -> [Var]
uvF (Function _ _ ss _) = concat $ map uvS ss


assToTailcalls :: Var -> Opt
assToTailcalls v = Opt {
    optStm = tceStm v,
    optExp = id
  }

tceStm :: Var -> Stmt -> Stmt
tceStm v (Assign v' (Call n f xs)) | v == v' =
  Tailcall f xs
tceStm v (AssignResult v' (Call n f xs)) | v == v' =
  Tailcall f xs
tceStm v x@(SelfThunk _ _) = error $ show x
tceStm _ x = x

retToTailcalls :: Opt 
retToTailcalls = Opt {
    optStm = tceStm2,
    optExp = id
  }

tceStm2 :: Stmt -> Stmt
tceStm2 (Return _ (Call n f xs)) =
  Tailcall f xs
tceStm2 x = x


eliminateTailCalls :: Function -> Function
eliminateTailCalls f =
  foldl' eliminateTC (optimize retToTailcalls f) vs
  where
    vs = uselessVars f
    eliminateTC f v = optimize (assToTailcalls v) f
