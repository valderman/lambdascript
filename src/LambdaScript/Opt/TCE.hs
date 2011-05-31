-- | Replace all instances of Return $ Call ... by Return Tailcall
module LambdaScript.Opt.TCE (eliminateTailCalls) where
import Data.List (intersect, foldl', union)
import Data.Maybe (maybeToList)
import LambdaScript.CodeGen.Ops
import LambdaScript.CodeGen.Module
import LambdaScript.Opt.Core

uselessVars :: [Var] -> Function -> [Var]
uselessVars = uvF

uvS :: [Var] -> Stmt -> [Var]
uvS vs (Assign v (Ident v')) | v `elem` vs =
  [v']
uvS vs (Assign _ exp) =
  uvE vs exp
uvS vs (AssignResult v (Ident v')) | v `elem` vs =
  [v']
uvS vs (AssignResult v (StmtEx _ (Ident v'))) | v `elem` vs =
  [v']
uvS vs (AssignResult _ exp) =
  uvE vs exp
uvS vs (If ex s1 s2) =
  uvE vs ex ++ uvS vs s1 ++ (concat $ maybeToList $ fmap (uvS vs) s2)
uvS vs (Return n (Ident v))        = [v]
uvS vs (Return n (Eval (Ident v))) = [v]
uvS vs (Return n (StmtEx ss (Ident v))) =
  v:concat (map (uvS vs) ss)
uvS vs (Return n (StmtEx ss (Eval (Ident v)))) =
  v:concat (map (uvS vs) ss)
uvS vs (Return n x)                = uvE vs x
uvS vs (Block ss)                  = concat $ map (uvS vs) ss
uvS vs (ExpStmt ex)                = uvE vs ex
uvS vs (Forever s)                 = uvS vs s
uvS _ _                           = []

uvE :: [Var] -> Exp -> [Var]
uvE vs (Thunk ex) = uvE vs ex
uvE vs (IOThunk ex) = uvE vs ex
uvE vs (Eval ex) = uvE vs ex
uvE vs (StmtEx ss ex) = concat $ uvE vs ex : map (uvS vs) ss
uvE vs (Index e1 e2) = uvE vs e1 ++ uvE vs e2
uvE vs (Array exs) = concat $ map (uvE vs) exs
uvE vs (ConstrIs e _ _) = uvE vs e
uvE vs (Cons e1 e2) = uvE vs e1 ++ uvE vs e2
uvE vs (Oper _ e1 e2) = uvE vs e1 ++ uvE vs e2
uvE vs (Neg e) = uvE vs e
uvE vs (Call _ e es) = concat $ map (uvE vs) (e:es)
uvE vs (FunExp (Lambda _ s)) = uvS vs s
uvE _ _ = []

uvF :: [Var] -> Function -> [Var]
uvF vs (Function _ _ ss _) = concat $ map (uvS vs) ss


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
    vs = uvars f []
    -- Any var that's returned without being part of a larger expression
    -- is a "useless" variable; if its value is the return value of a function
    -- call, that call is a candidate for tail call elimination.
    uvars f uv = let uv' = uv `union` uselessVars uv f in
      if length uv /= length uv'
         then uvars f uv'
         else uv'
    eliminateTC f v = optimize (assToTailcalls v) f
