-- | Module responsible for desugaring certain language constructs into
--   other, simpler, constructs.
module LambdaScript.Desugar (desugar) where
import Data.List
import LambdaScript.Abs
import LambdaScript.Print

-- | Prefix all freshly generated variables with this, to avoid name clashes.
generatedVarPrefix :: Char
generatedVarPrefix = '*'

-- | Perform desugaring on the given program.
desugar :: Program -> Program
desugar (Program defs) =
  Program $ desuDefs defs

-- | Desugar a list of definitions.
desuDefs :: [Def] -> [Def]
desuDefs defs =
  other ++ (map deFunDefs $ groupDefs funs)
  where
    (funs, other) = partition isFunDef defs
    isFunDef (FunDef _ _) = True
    isFunDef _            = False

-- | Group function definitions by identifier.
groupDefs :: [Def] -> [[Def]]
groupDefs =
  groupBy nameEq . sortBy nameOrder
  where
    nameOrder (FunDef id1 _) (FunDef id2 _) =
      id1 `compare` id2
    nameEq f1 f2 =
      nameOrder f1 f2 == EQ

-- | Desugar all definitions for a given symbol.
--   We call desuExpr after mergeCases because then we can recursively get
--   all the expressions with one call, as opposed to hunting down every
--   occurrence of an Expr somewhere else and desugaring it there.
deFunDefs :: [Def] -> Def
deFunDefs defs =
  Const $ ConstDef (Ident id) (lambdafy (map desuPat patsInFirst)
                              $ desuExpr
                              $ mergeCases
                              $ map casefy thePatExprs)
  where
    (id, patsInFirst) =
      case head defs of
        (FunDef (VIdent id) (TPNoGuards pats _)) ->
          (id, pats)
        (FunDef (VIdent id) (TPGuards pats _)) ->
          (id, pats)
    thePatExprs =
      map getPatExprs defs
    getPatExprs (FunDef _ tp) =
      tp

-- | Merge a list of case expressions into one case expression.
--   For example:
--    case a of 1 -> 1
--    case a of 2 -> 2
--  ...turns into:
--    case a of
--      1 -> 1
--      2 -> 2
mergeCases :: [Expr] -> Expr
mergeCases [soleCase] =
  soleCase
mergeCases cases =
  case (foldr addCase [] cases) of
    [] -> desuExpr expr
    cs -> ECase (desuExpr expr) cs
  where
    expr =
      -- If this isn't a case expression then we're dealing with multiple
      -- definitions of the same constant, and this is the first definition.
      -- The first definition overshadows later definitions, so we keep it.
      case head cases of
        (ECase ex _) -> ex
        ex           -> ex
    addCase (ECase _ cps) a =
      cps ++ a
    -- if this is something other than a case, we tried to merge two constant
    -- definitions. perfectly legal, but we just drop it since the first one
    -- overshadows later ones anyway.
    addCase _ a = a

-- | Turn a list of function definition patterns into a case expression with
--   a tuple. For example:
--     fun a b c = expr
--   ...turns into:
--     case (v0, v1, v2) of (a, b, c) -> expr
casefy :: TopPattern -> Expr
casefy (TPNoGuards [] ex) = ex
casefy (TPGuards pats guards) =
  ECase (pats2Tuple pats) [CPGuards (PTuple $ map PatC pats) (top2case guards)]
  where
    top2case = map (\(GuardedTopExpr guard ex) -> GuardedCaseExpr guard ex)
casefy (TPNoGuards pats ex) =
  ECase (pats2Tuple pats) [CPNoGuards (PTuple $ map PatC pats) ex]  


-- | Calculate the first hand dependencies of the given expression.
--   First hand, as in only the explicit, non-recursive dependencies of this
--   expression.
desuExpr :: Expr -> Expr
desuExpr = go
  where
    go (EList exs)       = EList (map desuExpr exs)
    go (ETuple exs)      = ETuple (map desuExpr exs)
    go (EApp f x)        = EApp (desuExpr f) (desuExpr x) 
    go (ECons x xs)      = ECons (desuExpr x) (desuExpr xs)
    go (EConcat xs ys)   = EConcat (desuExpr xs) (desuExpr ys)
    go (EMul a b)        = EMul (desuExpr a) (desuExpr b)
    go (EDiv a b)        = EDiv (desuExpr a) (desuExpr b)
    go (EAdd a b)        = EAdd (desuExpr a) (desuExpr b)
    go (ESub a b)        = ESub (desuExpr a) (desuExpr b)
    go (EMod a b)        = EMod (desuExpr a) (desuExpr b)
    go (EEq a b)         = EEq (desuExpr a) (desuExpr b)
    go (ELT a b)         = ELT (desuExpr a) (desuExpr b)
    go (EGT a b)         = EGT (desuExpr a) (desuExpr b)
    go (ELE a b)         = ELE (desuExpr a) (desuExpr b)
    go (EGE a b)         = EGE (desuExpr a) (desuExpr b)
    go (ENE a b)         = ENE (desuExpr a) (desuExpr b)
    go (EAnd a b)        = EAnd (desuExpr a) (desuExpr b)
    go (EOr a b)         = EOr (desuExpr a) (desuExpr b)
    go (ENot a)          = ENot (desuExpr a)
    go (ELambda v x)     = desuLambda v (desuExpr x)
    go (EIf a b c)       = EIf (desuExpr a) (desuExpr b) (desuExpr c)
    go (ECase ex pats)   = ECase (desuExpr ex) (map desuCasePat pats)
    go (EBinds ex defs)  = EBinds (desuExpr ex) (desuDefs defs)
    go (EDo stmts)       = desuDoBlock stmts
    go expr              = expr

-- | Desugar a do-block.
--   do a; b; turns into bind a (\_ -> b)
--   do x <- a; b; turns into bind a (\x -> b)
desuDoBlock :: [DoStmt] -> Expr
desuDoBlock stmts =
  case reverse stmts of
    (DoArrow _ _:_) ->
      error "The last statement in a do-block must be an expression!"
    ((JustDo final):rest) ->
      -- This should have been a foldr, but since we had to inspect the last
      -- statement anyway we might as well reverse the list and to a foldl'
      -- on it instead.
      foldl' desuStmt (desuExpr final) rest
  where
    desuStmt next (DoArrow id ex) =
      EApp (EApp (EVar $ VIdent "_bind") (desuExpr ex)) (ELambda [PID id] next)
    desuStmt next (JustDo ex) =
      EApp (EApp (EVar $ VIdent "_bind") (desuExpr ex)) (ELambda [PWildcard] next)

-- | Desugar a lambda expression; basically turns \a b -> ... into
--   \a -> \b -> ...
desuLambda :: [Pattern] -> Expr -> Expr
desuLambda [v] ex =
  ELambda [v] ex
desuLambda (v:vs) ex =
  ELambda [v] (desuLambda vs ex)

-- | Desugar a case pattern.
desuCasePat :: CasePattern -> CasePattern
desuCasePat (CPGuards pat guardeds) =
  CPGuards (desuPat pat) (map desuGuarded guardeds)
desuCasePat (CPNoGuards pat expr) =
  CPNoGuards (desuPat pat) (desuExpr expr)

-- | Desugar a guarded case expression.
desuGuarded :: GuardedCaseExpr -> GuardedCaseExpr
desuGuarded (GuardedCaseExpr (GuardExpr guard) expr) =
  GuardedCaseExpr (GuardExpr $ desuExpr guard) (desuExpr expr)

-- | Desugar a pattern.
desuPat :: Pattern -> Pattern
desuPat (PList pats)      = PList $ map (\(PatC p) -> PatC $ desuPat p) pats
desuPat (PTuple pats)     = PTuple $ map (\(PatC p) -> PatC $ desuPat p) pats
desuPat (PConstr id pats) = PConstr id $ map desuPat pats
desuPat (PCons p1 p2)     = PCons (desuPat p1) (desuPat p2)
desuPat pat               = pat

-- | Creates a tuple expression for pattern matching on function arguments in
--   the function body redurned by lambdafy.
pats2Tuple :: [Pattern] -> Expr
pats2Tuple pats =
  ETuple [EVar $ VIdent $ generatedVarPrefix:show n | (_, n) <- zip pats [0..]]

-- | Turn a list function definition split into patterns and expression into
--   an equivalent set of nested lambdas.
--   For example:
--     fun a b c = expr
--   ...turns into:
--     fun = \v0 -> \v1 -> \v2 -> expr
--   v0, v1..vn are represented in code as *0, *1, etc. to make sure we handle
--   expressions that incorrectly use an unbound identifier v0 as we should.
--   (That is, by rejecting the expression.)
lambdafy :: [Pattern] -> Expr -> Expr
lambdafy pats ex =
  foldr mkLambda ex (zip pats [0..])
  where
    mkLambda (_, n) a =
      ELambda [PID $ VIdent $ generatedVarPrefix:show n] a
