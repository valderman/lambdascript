-- | Module responsible for dividing the program into mutual recursion groups
--   and sorting said groups in dependency order.
module LambdaScript.Depends (bindGroups) where
import Prelude as P
import Data.Map as M
import Data.Set as S
import Data.List as L
import Data.Maybe
import Control.Monad.State
import LambdaScript.Abs

-- | Group all function definitions in the AST by mutual recursion group
--   (mutually recursive functions form a single unit during type checking;
--   there can be no polymorphism between them) then sort said groups by
--   dependency. For example:
--     f x = g h
--     g x = h x
--     h x = g (i x)
--     i x = x + x
--   ...will turn into the following list of groups:
--     [[i], [g, h], [f]]
--   g and h are mutually recursive; one of them depends on i, so both of them
--   do. Finally, f depends on g, which again is an atomic unit with h.
--   
--   Note that bindGroups expects its input to have already been desugared.
--   As such, the AST must not contain any FunDefs (they should have been
--   turned into Consts.)
bindGroups :: Program -> Program
bindGroups (Program defs) =
  Program $ typeDefDecls ++ L.map (BGroup . mkBindGroup) (depSort defPairs)
  where
    -- Separate out the desugared function definitions.
    (fundefs, typeDefDecls) = L.partition isConst defs
    isConst (Const _) = True
    isConst _         = False

    -- Turn the ConstDefs into pairs
    defPairs =
      foldr const2pair [] fundefs
    const2pair (Const (ConstDef (Ident id) ex)) a =
      (id, ex) : a
    const2pair _ a =
      a

    -- Create a map of function names to expressions.
    funs =
      M.fromList defPairs

    -- Make a BindGroup out of a set of ids
    mkBindGroup ids =
      BindGroup $ L.map id2const ids
    id2const id =
      ConstDef (Ident id) (exprBGs $ funs ! id)

-- | Perform dependency analysis, grouping and sorting within the given
--   expression. This basically consists of hunting down bind clauses and
--   giving them the same treatment as the top level environment.
exprBGs :: Expr -> Expr
exprBGs (EList exs) =
  EList $ L.map exprBGs exs
exprBGs (ETuple exs) =
  ETuple $ L.map exprBGs exs
exprBGs (EApp e1 e2) =
  EApp (exprBGs e1) (exprBGs e2)
exprBGs (ECons e1 e2) =
  ECons (exprBGs e1) (exprBGs e2)
exprBGs (EConcat e1 e2) =
  EConcat (exprBGs e1) (exprBGs e2)
exprBGs (EMul e1 e2) =
  EMul (exprBGs e1) (exprBGs e2)
exprBGs (EDiv e1 e2) =
  EDiv (exprBGs e1) (exprBGs e2)
exprBGs (EMod e1 e2) =
  EMod (exprBGs e1) (exprBGs e2)
exprBGs (EAdd e1 e2) =
  EAdd (exprBGs e1) (exprBGs e2)
exprBGs (ESub e1 e2) =
  ESub (exprBGs e1) (exprBGs e2)
exprBGs (EEq e1 e2) =
  EEq (exprBGs e1) (exprBGs e2)
exprBGs (ELT e1 e2) =
  ELT (exprBGs e1) (exprBGs e2)
exprBGs (EGT e1 e2) =
  EGT (exprBGs e1) (exprBGs e2)
exprBGs (ELE e1 e2) =
  ELE (exprBGs e1) (exprBGs e2)
exprBGs (EGE e1 e2) =
  EGE (exprBGs e1) (exprBGs e2)
exprBGs (ENE e1 e2) =
  ENE (exprBGs e1) (exprBGs e2)
exprBGs (ELambda pats ex) =
  ELambda pats (exprBGs ex)
exprBGs (EIf a b c) =
  EIf (exprBGs a) (exprBGs b) (exprBGs c)
exprBGs (ECase ex cps) =
  ECase (exprBGs ex) (L.map cpBGs cps)
exprBGs (EBinds ex defs) =
  EBinds (exprBGs ex) (((\(Program defs) -> defs). bindGroups . Program) defs)
-- The rest can't contain expressions or bind groups, so we don't care about
-- them.
exprBGs ex =
  ex

-- | Same as for 'exprBGs' but for case patterns.
cpBGs :: CasePattern -> CasePattern
cpBGs (CPGuards p guardeds) =
  CPGuards p $ L.map gceBGs guardeds
  where
    gceBGs (GuardedCaseExpr (GuardExpr ge) e) =
      GuardedCaseExpr (GuardExpr $ exprBGs ge) $ exprBGs e
cpBGs (CPNoGuards p ex) =
  CPNoGuards p (exprBGs ex)

-- | Group a list of (Ident, Expr) by mutual recursion, then sort all such
--   units by dependency.
depSort :: [(String, Expr)] -> [[String]]
depSort syms = concat sorted
  where
    depsyms = depends syms
    groups  = depGroups depsyms
    depmap  = depToDepGroup depsyms groups

    sorted  = evalState (mapM (sort []) groups) depmap
    sort acc id = do
      deps <- get
      case M.lookup id deps of
        Just d -> do
          put $ M.delete id deps
          lists <- mapM (sort (id:acc)) d
          return $ foldr L.union [id] lists
        _     -> do
          return []

-- | Calculate the first hand dependencies of the given expression.
--   First hand, as in only the explicit, non-recursive dependencies of this
--   expression.
depExpr :: Expr -> [String]
depExpr = go
  where
    go (EVar (VIdent x)) = [x]
    go (EList exs)       = foldr L.union [] (L.map go exs)
    go (ETuple exs)      = foldr L.union [] (L.map go exs)
    go (EApp f x)        = go f `L.union` go x 
    go (ECons x xs)      = go x `L.union` go xs
    go (EConcat a b)     = go a `L.union` go b
    go (EMul a b)        = go a `L.union` go b
    go (EDiv a b)        = go a `L.union` go b
    go (EAdd a b)        = go a `L.union` go b
    go (ESub a b)        = go a `L.union` go b
    go (EMod a b)        = go a `L.union` go b
    go (EEq a b)         = go a `L.union` go b
    go (ELT a b)         = go a `L.union` go b
    go (EGT a b)         = go a `L.union` go b
    go (ELE a b)         = go a `L.union` go b
    go (EGE a b)         = go a `L.union` go b
    go (ENE a b)         = go a `L.union` go b
    go (ELambda v x)     = go x
    go (EIf a b c)       = go a `L.union` go b `L.union` go c
    go (ECase ex pats)   = foldr L.union (go ex) (L.map depCasePat pats)
    go (EBinds ex defs)  = foldr L.union (go ex) (L.map depDef defs)
    go _                 = []

-- | Calculate the first hand (non-recursively) dependencies of a case pattern.
depCasePat :: CasePattern -> [String]
depCasePat (CPGuards pat guardeds) =
  foldr L.union (depPat pat) (L.map depGuarded guardeds)
  where
    depGuarded (GuardedCaseExpr (GuardExpr guard) expr) =
      depExpr guard `L.union` depExpr expr 
depCasePat (CPNoGuards pat ex) =
  depPat pat `L.union` depExpr ex

-- | Calculate the first hand dependencies (that is, non-recursive) of a
--   pattern.
depPat :: Pattern -> [String]
depPat (PID (VIdent id)) = [id]
depPat (PList pats)      = foldr (\(PatC p) a -> depPat p `L.union` a) [] pats
depPat (PTuple pats)     = foldr (\(PatC p) a -> depPat p `L.union` a) [] pats
depPat (PConstr id pats) = foldr L.union [] (L.map depPat pats)
depPat (PCons p1 p2)     = depPat p1 `L.union` depPat p2
depPat _                 = []

-- | Calculate the first hand dependencies of a definition.
--   We only deal with Const constructors here, as the AST is expected to be
--   desugared at this point.
depDef :: Def -> [String]
depDef (Const (ConstDef _ expr)) =
  depExpr expr

-- | Calculate what symbols every named expression depends on.
depends :: [(String, Expr)] -> Map String [String]
depends = foldr (\(id, ex) a -> M.insert id (depExpr ex) a) M.empty

-- | Turns a symbol -> dependencies mapping into a group -> depended-on groups
--   mapping.
depToDepGroup :: Map String [String]     -- ^ Symbol -> deps mapping.
              -> [[String]]              -- ^ All dependency groups.
              -> Map [String] [[String]] -- ^ Group -> depended-on groups.
depToDepGroup deps groups =
  foldr (\x a -> M.insert x (findGroupDeps x) a) M.empty groups 
  where
    -- Create a symbol => dependency group mapping for every symbol in a group.
    insertGroup grp  acc  = foldr (flip M.insert grp) acc grp
    -- A symbol => dependency group mapping for all dependency groups.
    groupMap              = foldr insertGroup M.empty groups
    -- Find all groups the given group depends on.
    findGroupDeps grp     = nub $ L.map (lookupListSafe groupMap)
                                $ concat
                                $ L.map (lookupListSafe deps) grp
    -- If we don't find any dependencies for a given identifier, then it
    -- doesn't have any that we need to care about. This can happen with,
    -- for instance, local variables or out-of-module functions.
    lookupListSafe m k =
      case M.lookup k m of
        Just xs -> xs
        _       -> []

-- | Finds all dependency groups in the given environment.
depGroups :: Map String [String] -> [[String]]
depGroups m = mergeList                  -- Merge overlapping groups.
            $ concat                     -- Turn them into one big list.
            $ L.map (rec' []) (M.keys m) -- Find recursion groups.
  where
    rec' acc id
      | id `elem` acc =
        -- If we've seen this id before, we have recursion. Then we just need
        -- to drop any functions that might have called into the recursion
        -- chain before recursion started. For example, if we have:
        --   f x = g x
        --   g x = h x
        --   h x = g x
        -- ...then f is not part of the recursion chain, but forms its own
        -- dependency group that in turn depends on the [f, g] group.
        [dropWhile (/= id) (reverse acc)]
      | otherwise =
        -- Merge the results of recursively calculating depgroups for id's
        -- dependencies, adding id itself to the mix.
        -- If the dependency isn't found in the map, we simply ignore it.
        -- If this indicates an error it'll be found during type checking,
        -- and the condition might arise naturally anyway, for functions that
        -- depend on out of module functions (for example, standard library
        -- functions) as the types of those are always known and so don't need
        -- to be taken into account when calculating the order of dependencies.
        case M.lookup id m of
          Just idDeps ->
              foldr L.union [[id]] $ L.map (rec' (id:acc)) idDeps
          _           ->
              []

    -- Merge all overlapping dependency groups; we only try to merge groups
    -- with more than one member to cut down on the comparisons, as singletons
    -- will only ever overlap themselves.
    mergeList xs =
      foldr tryMerge xs (L.filter ((>1) . length) xs)

    -- Merge a dependency group with any overlapping group in a list.
    tryMerge x ys =
        foldr (\y a -> if not $ L.null $ intersect x y
                         then (x `L.union` y) : (L.delete x $ L.delete y a)
                         else (y:a)) [] ys
