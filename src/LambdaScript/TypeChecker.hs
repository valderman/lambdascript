module LambdaScript.TypeChecker (infer) where
import Control.Monad
import Data.List hiding (find)
import LambdaScript.Abs
import LambdaScript.Types
import LambdaScript.TCM

-- | Infer types for the whole progran.
infer :: Program -> (Program, Subst)
infer (Program defs) =
  case runTCM $ tiDefs [] $ defs of
    ((_, defs'), subst) -> (Program defs', subst)

-- | Infer the type of a list of definitions.
tiDefs :: Infer [Def] [Assump]
tiDefs as defs =
  foldM (\(a, ds) d -> do
            (as', d') <- tiDef a d
            return (as', d':ds))
        (as, [])
        defs

-- | Infer the types of a definition, which at this point should be either a
--   bind group, a type declaration or a type definition.
tiDef :: Infer Def [Assump]
tiDef as (BGroup g) = do
  (as', bg) <- tiBindGroup as g
  return (as', BGroup bg)
tiDef as d@(TypeDecl _ _) = return (as, d) -- TODO: add constructor types here
tiDef _ d =
  fail $ "No implementation for inferring: " ++ show d

-- | Infer the type of an implicit bind group.
tiBindGroup :: Infer BindGroup [Assump]
tiBindGroup as (BindGroup defs) = do
  ts <- mapM (\_ -> newTVar Star) defs
  let isexs     = map (\(ConstDef (Ident id) ex) -> (id, ex)) defs
      (is, exs) = unzip isexs
      scs       = map toScheme ts
      as'       = zipWith (:>:) is scs ++ as
  exs' <- sequence $ zipWith (\ex t -> do
                                 (_, ex') <- tiExpr as' ex
                                 unify (eUntyped ex') t
                                 return ex') exs ts
  s <- getSubst
  -- Apply the global substitution to the types we got back, then make a list
  -- of all free type vars and quantify over them. Finally, add assumptions for
  -- all expressions in bind group.
  let ts'  = apply s ts
      vs   = foldr1 union (map freeVars ts') \\ freeVars (apply s as)
      scs' = map (quantify vs) ts'
      as'' = zipWith (:>:) is scs' ++ as
  return (as'', BindGroup $ zipWith ConstDef (map Ident is) exs')

-- | Infer the type of an expression.
tiExpr :: Infer Expr [Assump]
tiExpr as e@(EConstr (TIdent id)) = do
  t <- find id as >>= instantiate
  return (as, eTyped e t)
tiExpr as e@(EVar (VIdent id)) = do
  t <- find id as >>= instantiate
  return (as, eTyped e t)
tiExpr as e@(EInt _) = do
  return (as, eTyped e tInt)
tiExpr as e@(EDoub _) = do
  return (as, eTyped e tDouble)
tiExpr as e@(EStr _) = do
  return (as, eTyped e tString)
tiExpr as e@(EChar _) = do
  return (as, eTyped e tChar)
tiExpr as (EList exs) = do
  v <- newTVar Star
  exs' <- mapM (\ex -> tiExpr as ex >>= return . snd) exs
  mapM_ (unify v) (map eUntyped exs')
  return (as, eTyped (EList exs') v)
tiExpr as (ETuple exs) = do
  exs' <- mapM (\ex -> tiExpr as ex >>= return . snd) exs
  return (as, eTyped (ETuple exs') (tuple $ map eUntyped exs'))
tiExpr as (EApp f x) = do
  t <- newTVar Star
  (_, f') <- tiExpr as f
  (_, x') <- tiExpr as x
  unify (eUntyped f') (eUntyped x' ~> t)
  return (as, eTyped (EApp f' x') t)
tiExpr as (ECons x xs) = do
  (_, x') <- tiExpr as x
  (_, xs') <- tiExpr as xs
  unify (list $ eUntyped x') (eUntyped xs')
  return (as, eTyped (ECons x' xs') (eUntyped xs'))  
tiExpr as (EConcat xs ys) = do
  (_, xs') <- tiExpr as xs
  (_, ys') <- tiExpr as ys
  v <- newTVar Star
  unify (list v) (eUntyped xs')
  unify (list v) (eUntyped ys')
  return (as, eTyped (EConcat xs' ys') (list v))
tiExpr as (EMul x y)  = tiNumOp as EMul x y
tiExpr as (EMod x y)  = tiNumOp as EMod x y
tiExpr as (EAdd x y)  = tiNumOp as EAdd x y
tiExpr as (ESub x y)  = tiNumOp as ESub x y
tiExpr as (EDiv x y)  = tiNumOp as EDiv x y
tiExpr as (EEq x y)   = tiComparison as EEq x y
tiExpr as (ELT x y)   = tiComparison as ELT x y
tiExpr as (EGT x y)   = tiComparison as EGT x y
tiExpr as (ELE x y)   = tiComparison as ELE x y
tiExpr as (ENE x y)   = tiComparison as ENE x y
tiExpr as (EGE x y)   = tiComparison as EGE x y
tiExpr as (ECase e c) = tiCase as (ECase e c)
tiExpr as (ELambda pats ex) = do
  (as', pats') <- tiPats as pats
  (_, ex') <- tiExpr as' ex
  let t = foldr1 (~>) $ map pUntyped pats' ++ [eUntyped ex']
  return (as, eTyped (ELambda pats' ex') t)
tiExpr as (EIf cond thenEx elseEx) = do
  (_, cond') <- tiExpr as cond
  unify tBool (eUntyped cond')
  (_, thenEx') <- tiExpr as thenEx
  (_, elseEx') <- tiExpr as elseEx
  unify (eUntyped thenEx') (eUntyped elseEx')
  return (as, eTyped (EIf cond' thenEx' elseEx') (eUntyped thenEx'))
tiExpr as (EBinds ex defs) = do
  (as', defs') <- tiDefs as defs
  (_, ex') <- tiExpr as' ex
  return (as, eTyped (EBinds ex' defs') (eUntyped ex'))
tiExpr _ ex =
  error $ "Not implemented - inferring expression: " ++ show ex

-- | Infer the type of a case expression.
tiCase :: Infer Expr [Assump]
tiCase as e = do
  let (ECase ex cps) = e
  (_, ex') <- tiExpr as ex
  cps' <- mapM (tiCasePat (eUntyped ex') as) cps
  v <- newTVar Star
  let (ts, cps'') = unzip cps'
  mapM_ (unify v) ts
  return (as, eTyped (ECase ex' cps'') v)

-- | Infer the type of a case battern.
tiCasePat :: AbsType -> Infer CasePattern AbsType
tiCasePat exType as (CPNoGuards p ex) = do
  -- make sure the type of the pattern and the expression matched against
  -- are the same.
  (as', p') <- tiPat as p
  unify exType (pUntyped p')
  -- Infer expression, return annotated CasePattern and its return type.
  (_, ex') <- tiExpr as' ex
  return (eUntyped ex', CPNoGuards p' ex')
tiCasePat exType as (CPGuards p guardeds) = do
  (as', p') <- tiPat as p
  unify exType (pUntyped p')
  tsgs <- mapM (tiGCE as') guardeds
  let (ts, gs) = unzip tsgs
  v <- newTVar Star
  mapM_ (unify v) ts
  return (v, CPGuards p' gs)

-- | Infer the type of a guarded case expression.
tiGCE :: Infer GuardedCaseExpr AbsType
tiGCE as (GuardedCaseExpr (GuardExpr gex) ex) = do
  (_, gex') <- tiExpr as gex
  unify tBool (eUntyped gex')
  (_, ex') <- tiExpr as ex
  return (eUntyped ex', GuardedCaseExpr (GuardExpr gex') ex')

-- | Infer types for an overloaded arithmetic operation.
tiNumOp :: [Assump] -> (Expr->Expr->Expr) -> Expr -> Expr -> TCM([Assump],Expr)
tiNumOp as cons x y = do
  (_, x') <- tiExpr as x
  (_, y') <- tiExpr as y
  t <- newTVar Star >>= return . num
  unify t (eUntyped x')
  unify t (eUntyped y')
  return (as, eTyped (cons x' y') t)

-- | Infer types for a comparison (EQ, LT, etc.) operator.
tiComparison :: [Assump]
             -> (Expr -> Expr -> Expr)
             -> Expr
             -> Expr
             -> TCM([Assump],Expr)
tiComparison as cons x y = do
  (_, x') <- tiExpr as x
  (_, y') <- tiExpr as y
  unify (eUntyped x') (eUntyped y')
  return (as, eTyped (cons x' y') tBool)

-- | Helper to type annotate a pattern.
pTyped :: Pattern -> AbsType -> Pattern
pTyped p at = PTyped p (a2t at)

-- | Helper to extract a type from an annotated pattern.
pUntyped :: Pattern -> AbsType
pUntyped (PTyped _ t) = t2a t
pUntyped p            = error $ "Can't untype: " ++ show p

-- | Helper to type annotate an expression.
eTyped :: Expr -> AbsType -> Expr
eTyped e at = ETyped e (a2t at)

-- | Helper to extract a type from an annotated expression.
eUntyped :: Expr -> AbsType
eUntyped (ETyped _ t) = t2a t
eUntyped p            = error $ "Can't untype: " ++ show p

-- | Infer the type of a pattern.
tiPat :: Infer Pattern [Assump]
tiPat as p@(PID (VIdent id)) = do
  v <- newTVar Star
  return (id :>: toScheme v:as, pTyped p v)
tiPat as p@(PInt n) = do
  return (as, pTyped p tInt)
tiPat as p@(PDoub d) = do
  return (as, pTyped p tDouble)
tiPat as p@(PChar c) = do
  return (as, pTyped p tChar)
tiPat as p@(PString s) = do
  return (as, pTyped p tString)
tiPat as p@(PWildcard) = do
  v <- newTVar Star
  return (as, pTyped p v)
tiPat as (PConstr (TIdent id) pats) = do
  -- lookup type of constructor
  t <- find id as >>= instantiate
  -- infer types for all constructor args
  -- TODO: check that we have the correct # of args!
  (as', ps) <- tiPats as pats
  let ts = map pUntyped ps
  t' <- newTVar Star
  -- unify instantiated type with the inferred arg types
  unify t (foldr (~>) t' ts)
  return (as', pTyped (PConstr (TIdent id) ps) t')
tiPat as (PList pats) = do
  -- create a new type variable as a representative for the content type of the
  -- list.
  v <- newTVar Star
  -- infer types for all patterns within list
  (as', ps) <- tiPats as (map (\(PatC p) -> p) pats)
  -- make sure the types of all arguments unify with each other
  mapM_ (unify v) (map pUntyped ps)
  -- return a list of the result type of that mass unification as the
  -- representative type of this pattern.
  return (as', pTyped (PList $ map PatC ps) (list v))
tiPat as (PTuple pats) = do
  (as', ps) <- tiPats as (map (\(PatC p) -> p) pats)
  return (as', pTyped (PTuple $ map PatC ps) (tuple $ map pUntyped ps))
tiPat as (PCons p ps) = do
  -- infer type of x pattern in x:xs
  (as', p') <- tiPat as p
  -- infer type of xs part in x:xs
  (as'', ps') <- tiPat as' ps
  let tlist = pUntyped ps'
  -- make sure that type of x unifies with the type of xs
  unify (list $ pUntyped p') tlist
  -- return a list of the type of x as the representative type for this pat.
  return (as'', pTyped (PCons p' ps') tlist)

-- | Infer types for a list of patterns.
tiPats :: Infer [Pattern] [Assump]
tiPats as ps = do
  (as', ps') <- foldM pat (as, []) ps
  return (as', ps')
  where
    pat (as, ps) x = do
      (as', p') <- tiPat as x
      return (as', p':ps)
