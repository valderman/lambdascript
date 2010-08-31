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
  Program $ map deFunDefs $ groupDefs (filter isFunDef defs)
  where
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
deFunDefs :: [Def] -> Def
deFunDefs defs =
  Const $ ConstDef (Ident id) (lambdafy patsInFirst
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

-- | Desugar a lone function definition.
deFun :: Def -> Def
deFun (FunDef id tp@(TPGuards pats guards)) =
  FunDef id (TPNoGuards [] (lambdafy pats $ casefy tp))
deFun (FunDef id tp@(TPNoGuards pats ex)) =
  FunDef id (TPNoGuards [] (lambdafy pats $ casefy tp))

-- | Merge a list of case expressions into one case expression.
--   For example:
--    case a of 1 -> 1
--    case a of 2 -> 2
--  ...turns into:
--    case a of
--      1 -> 1
--      2 -> 2
mergeCases :: [Expr] -> Expr
mergeCases cases =
  ECase expr (foldr addCase [] cases)
  where
    expr =
      case head cases of (ECase ex _) -> ex
    addCase (ECase _ cps) a =
      cps ++ a

-- | Turn a list of function definition patterns into a case expression with
--   a tuple. For example:
--     fun a b c = expr
--   ...turns into:
--     case (v0, v1, v2) of (a, b, c) -> expr
casefy :: TopPattern -> Expr
casefy (TPGuards pats guards) =
  ECase (pats2Tuple pats) [CPGuards (PTuple $ map PatC pats) (top2case guards)]
  where
    top2case = map (\(GuardedTopExpr guard ex) -> GuardedCaseExpr guard ex)
casefy (TPNoGuards pats ex) =
  ECase (pats2Tuple pats) [CPNoGuards (PTuple $ map PatC pats) ex]  

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
