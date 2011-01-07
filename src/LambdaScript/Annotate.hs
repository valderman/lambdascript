-- | Annotate the parse tree with the most concrete types possible.
module LambdaScript.Annotate (annotate) where
import LambdaScript.Abs
import LambdaScript.Types

-- | Resolve a type to its most concrete representation.
resolve :: Subst -> Type -> Type
resolve s (TLst t)     = TLst (resolve s t)
resolve s (TTup ts)    = TTup (map (resolve s) ts)
resolve s (TApp t1 t2) = TApp (resolve s t1) (resolve s t2)
resolve s (TOp t1 t2)  = TOp (resolve s t1) (resolve s t2)
resolve s (TVar v)     = find v s
  where
    find v ((v', t):xs)
      | v == v' =
        resolve s t
      | otherwise =
        find v xs
    find v s =
      TVar v
resolve s t            = t

annotate :: Subst -> Program -> Program
annotate s (Program defs) =
  Program $ map (anDef s) defs

-- | Annotate definitions; only bind groups are of interest here as everything
--   else is either a typedef or type declaration, or desugared out of
--   existence.
anDef :: Subst -> Def -> Def
anDef s (BGroup (BindGroup bg)) =
  BGroup . BindGroup . map (\(ConstDef id ex) -> ConstDef id (anExp s ex)) $ bg
anDef s x           = x

-- | Annotate an expression.
anExp :: Subst -> Expr -> Expr
anExp s (ETyped e t)    = ETyped (anExp s e) (resolve s t)
anExp s (EList exs)     = EList (map (anExp s) exs)
anExp s (ETuple exs)    = ETuple (map (anExp s) exs)
anExp s (ENot e)        = ENot (anExp s e)
anExp s (EApp e1 e2)    = EApp (anExp s e1) (anExp s e2)
anExp s (ECons e1 e2)   = ECons (anExp s e1) (anExp s e2)
anExp s (EAnd e1 e2)    = EAnd (anExp s e1) (anExp s e2)
anExp s (EOr e1 e2)     = EOr (anExp s e1) (anExp s e2)
anExp s (EConcat e1 e2) = EConcat (anExp s e1) (anExp s e2)
anExp s (EMul e1 e2)    = EMul (anExp s e1) (anExp s e2)
anExp s (EDiv e1 e2)    = EDiv (anExp s e1) (anExp s e2)
anExp s (EMod e1 e2)    = EMod (anExp s e1) (anExp s e2)
anExp s (EAdd e1 e2)    = EAdd (anExp s e1) (anExp s e2)
anExp s (ESub e1 e2)    = ESub (anExp s e1) (anExp s e2)
anExp s (EEq e1 e2)     = EEq (anExp s e1) (anExp s e2)
anExp s (ELT e1 e2)     = ELT (anExp s e1) (anExp s e2)
anExp s (EGT e1 e2)     = EGT (anExp s e1) (anExp s e2)
anExp s (ELE e1 e2)     = ELE (anExp s e1) (anExp s e2)
anExp s (EGE e1 e2)     = EGE (anExp s e1) (anExp s e2)
anExp s (ENE e1 e2)     = ENE (anExp s e1) (anExp s e2)
anExp s (ELambda ps e)  = ELambda (map (anPat s) ps) (anExp s e)
anExp s (EIf a b c)     = EIf (anExp s a) (anExp s b) (anExp s c)
anExp s (ECase e cps)   = ECase (anExp s e) (map (anCP s) cps)
anExp s (EBinds e ds)   = EBinds (anExp s e) (map (anDef s) ds)
anExp s x               = x

anCP :: Subst -> CasePattern -> CasePattern
anCP s (CPGuards p gces) = CPGuards (anPat s p) (map (anGCE s) gces)
anCP s (CPNoGuards p e)  = CPNoGuards (anPat s p) (anExp s e)

anGCE :: Subst -> GuardedCaseExpr -> GuardedCaseExpr
anGCE s (GuardedCaseExpr (GuardExpr ge) e) =
  GuardedCaseExpr (GuardExpr (anExp s ge)) (anExp s e)

anPat :: Subst -> Pattern -> Pattern
anPat s (PTyped p t)    = PTyped p (resolve s t)
anPat s (PList ps)      = PList (map (\(PatC p) -> PatC (anPat s p)) ps)
anPat s (PTuple ps)     = PTuple (map (\(PatC p) -> PatC (anPat s p)) ps)
anPat s (PConstr id ps) = PConstr id (map (anPat s) ps)
anPat s (PCons p1 p2)   = PCons (anPat s p1) (anPat s p2)
anPat s x               = x