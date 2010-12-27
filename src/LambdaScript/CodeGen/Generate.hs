module LambdaScript.CodeGen.Generate (function, generate) where
import LambdaScript.Abs as A
import LambdaScript.CodeGen.Monad
import LambdaScript.CodeGen.Ops as Ops
import LambdaScript.CodeGen.Errors
import LambdaScript.CodeGen.Module
import LambdaScript.CodeGen.GenTypes
import qualified Data.Map as M
import Data.List (foldl1', foldl')
import Control.Monad (foldM)
import qualified LambdaScript.Builtins as Builtin

-- | Generate code for all functions in a program.
generate :: Program -> [Function]
generate p@(Program defs) =
  concat bgs
  where
    constrs =
      allTypesMap $ Program (map TypeDef Builtin.types ++ defs)
    -- this is just syntactic acrobatics to make writing a map with a multiline
    -- lambda a little less painful by giving the list to map over before the
    -- function.
    bgs =
      flip map defs $
        \x -> case x of
          BGroup (BindGroup d) ->
            map (gen function constrs env firstLocal) d
          _        ->
            []

    -- Assign IDs for every definition in every bind group.
    -- Also report the first free ID after assigning the globals.
    (env, firstLocal) =
      case foldl' numberGroups ([], 0) (Builtin.functions : defs) of
        (e, fl) -> (M.fromList e, fl)

    -- Assign IDs to all definitions in a single bind group.
    numberGroups acc (BGroup (BindGroup defs)) =
      foldl'
        (\(ids, n) (ConstDef (A.Ident id) _) ->
          ((id, n):ids, n+1)
        ) acc defs
    numberGroups acc _ =
      acc

-- | Code generator for a single function.
function :: Expr -> CG ()
function ex = genExpr ex >>= stmt . Return

-- | Smack an Eval primitive on an expression, provided that said expression
--   actually is a thunk.
eval :: Exp -> Exp
eval ex@(Thunk _) = Eval ex
eval ex           = ex

-- | Turn an expression into a thunk, without double-thunking it.
thunk :: Exp -> Exp
thunk ex@(Thunk _) = ex
thunk ex           = Thunk ex

-- | Generate lazy code for a binary operator.
oper :: Oper -> Expr -> Expr -> CG Exp
oper op a b = do
  a' <- genExpr a
  b' <- genExpr b
  return . thunk $ Oper op (eval a') (eval b')

-- | Create a conjunction of a bunch of expressions.
allTrue :: [Exp] -> Exp
allTrue = foldl1' (Oper And)

genExpr :: Expr -> CG Exp
genExpr (ETyped ex t) = genExpr' t ex
  where
    -- Constructrors and vars; just look them up
    genExpr' t (EConstr (TIdent id)) = do
      constrID id >>= return . FunExp . Construct t
    genExpr' t (EVar (VIdent id)) = do
      idOf id >>= return . Ops.Ident

    -- Constants; nothing strange here
    genExpr' t (EInt n) = do
      return . Ops.Const . NumConst . fromIntegral $ n
    genExpr' t (EDoub d) = do
      return . Ops.Const . NumConst $ d
    genExpr' t (EChar c) = do
      return . Ops.Const . CharConst $ c
    genExpr' t (EStr s) = do
      return . Ops.Const . strConst $ s
    
    -- Function application; generate code to obtain the function and to obtain
    -- the argument, then eval function and apply without evaluating argument.
    genExpr' t (EApp f x) = do
      f' <- genExpr f
      x' <- genExpr x
      return . thunk $ Call (eval f') [x']

    -- Various combinators for expressions; generate parts and thunk them.
    genExpr' t (EList (ex:exs)) = do
      ex' <- genExpr ex
      -- The entire list has the same type; use genExpr' instead of genExpr
      -- since EList exs is untyped and genExpr expects ETyped ...
      exs' <- genExpr' t (EList exs)
      return . thunk $ Cons ex' exs'
    genExpr' t (EList []) =
      return . thunk . Ops.Const $ EmptyListConst

    genExpr' t (ETuple elems) = do
      elems' <- mapM genExpr elems
      return . thunk $ Array elems'
    genExpr' t (ECons x xs) = do
      x' <- genExpr x
      xs' <- genExpr xs
      return . thunk $ Cons x' xs'
    genExpr' t (ENot ex) = do
      ex' <- genExpr ex
      return . thunk . Neg $ eval ex'
    genExpr' t (EConcat a b) = do
      a' <- genExpr a
      b' <- genExpr b
      return . thunk $ Call (FunExp $ FunIdent "concat") [a', b']
    
    -- Loooooong list of binary operators; boring!
    genExpr' t (EAnd a b) = oper And a b
    genExpr' t (EOr  a b) = oper Or  a b
    genExpr' t (EMul a b) = oper Mul a b
    genExpr' t (EDiv a b) = oper Div a b
    genExpr' t (EMod a b) = oper Mod a b
    genExpr' t (EAdd a b) = oper Add a b
    genExpr' t (ESub a b) = oper Sub a b
    genExpr' t (EEq  a b) = oper Eq  a b
    genExpr' t (ELT  a b) = oper Lt  a b
    genExpr' t (EGT  a b) = oper Gt  a b
    genExpr' t (ELE  a b) = oper Le  a b
    genExpr' t (EGE  a b) = oper Ge  a b
    genExpr' t (ENE  a b) = oper Ne  a b

    -- Lambda expression; generate it much as we would a case expression with
    -- a single alternative.
    genExpr' t (ELambda ps ex) = do
      return . thunk . FunExp =<< genLambda ps ex
      where
        genLambda ps ex = do
          (args, stmts) <- isolate $ do
            -- Create a var to refer to each argument
            vars <- mapM (\_ -> newVar) ps
            -- Generate pattern matching and binding code
            (exps, binds) <- isolate $ sequence $ zipWith genPat (map Ops.Ident vars) ps
            -- Generate code for the lambda body
            (body, stmts) <- isolate $ genExpr ex
            -- If everything matches, we execute the body. Otherwise we
            -- runtimefail.
            stmt $ If (allTrue exps)
                      (Block $ binds ++ stmts ++ [Return body])
                      (Just lambdaPatternMismatch)
            return vars
          return $ Lambda args (Block stmts)

    -- If expression; basically a thunked a ? b : c;
    genExpr' t (EIf cond ifE elE) = do
      cond' <- genExpr cond
      ifE' <- genExpr ifE
      elE' <- genExpr elE
      return . thunk $ IfExp (eval cond') ifE' elE'

    -- Case expression; works pretty much like a lambda with an arbitrary
    -- number of bodies. (See also: recursive documentation)
    genExpr' t (ECase ex cps) = do
      ex' <- genExpr ex
      v <- newVar
      genCPs ex' v cps >>= stmt
      return $ Ops.Ident v

    -- Bindings; we just generate them and assign them to local vars.
    -- Or at least we will when we get around to implementing them.
    genExpr' t (EBinds ex defs) = do
      error "NO BINDINGS YET!"
      mapM_ genDef defs
      genExpr ex
    
    -- Catch-all for anything we might have forgotten to implement.
    genExpr' t ex =
      error $ "No codegen for expression:\n" ++ show ex

-- We should never reach this definition; everything should be typed.
genExpr ex =
  error $ "Untyped expression: " ++ show ex

-- | Patterns; generate an expression to check whether the pattern is matched
--   or not, and variable bindings for each branch. The variable bindings are
--   emitted as statements, as opposed to returned. Bindings are emitted as
--   statements, which are obviously only safe to execute if the returned
--   expression returns true, so genPat should always be run within an isolate
--   expression. (That is, isolate $ genPat ...)
genPat :: Exp -- ^ The expression that's to be matched against the pattern.
       -> Pattern -- ^ The pattern to match against.
       -> CG Exp  -- ^ The boolean expression indicating match/no match.
genPat ex (PTyped p _) = do
  genPat ex p
genPat ex (PID (VIdent id)) = do
  v <- newVar
  stmt $ Assign v $ ex
  bind id v
  return $ Ops.Const $ BoolConst True
genPat ex (PInt n) = do
  return $ Oper Eq ex (Ops.Const $ NumConst $ fromIntegral n)
genPat ex (PDoub d) = do
  return $ Oper Eq ex (Ops.Const $ NumConst d)
genPat ex (PString s) = do
  return $ Oper Eq ex (Ops.Const $ strConst s)
genPat ex (PChar c) = do
  return $ Oper Eq ex (Ops.Const $ CharConst c)  
genPat ex (PList pats) = do
  genPList ex pats
genPat ex (PTuple pats) = do
  genPTuple 0 ex pats
genPat ex (PConstr (TIdent id) args) = do
  cid <- constrID id
  (_, cond) <- foldM (\(n, cond) p -> do
                         c <- genPat (Index ex $ Ops.Const $ NumConst n) p
                         return (n+1, Oper And cond c))
                     (1, (ex `ConstrIs` cid))
                     args
  return cond
genPat _ PWildcard = do
  return $ Ops.Const $ BoolConst True
genPat ex (PCons hp tp) = do
  let h = Index ex $ Ops.Const $ NumConst 0
  let t = Index ex $ Ops.Const $ NumConst 1  
  h' <- genPat h hp
  t' <- genPat t tp
  return $ Oper And h' t'

-- | Generate code for a tuple pattern match. Tuples are represented as JS
--   arrays, so to generate code to match them we just recurse through the
--   list of patterns, increasing an index each time, and AND together the
--   returned conditions. Bindings are emitted by genPat as usual.
genPTuple :: Int -> Exp -> [PatC] -> CG Exp
genPTuple n ex (PatC p : pats) = do
  let x = Index ex (Ops.Const $ NumConst $ fromIntegral n)
  x' <- genPat x p
  y <- genPTuple (n+1) ex pats
  return $ Oper And x' y
genPTuple _ _ [] =
  return $ Ops.Const $ BoolConst True

-- | Generate code for a list pattern match. Lists are represented as arrays
--   like [first, [second, rest]].
--   Like with 'genPTuple' we just recursively generate pattern matches and AND
--   them together.
genPList :: Exp -> [PatC] -> CG Exp
genPList ex (PatC p : pats) = do  
  let h = Index ex $ Ops.Const $ NumConst 0
  let t = Index ex $ Ops.Const $ NumConst 1
  h' <- genPat h p
  t' <- genPList t pats
  return $ Oper And h' t'
genPList _ [] =
  return $ Ops.Const $ BoolConst True

-- | Generate a set of case patterns. A case pattern is a pattern of the form
--   pattern [| guard] -> expression. The given Exp gives the variable that is
--   to be matched against the pattern, and the Var is the variable where the
--   result of the case expression will be placed.
genCPs :: Exp -> Var -> [CasePattern] -> CG Stmt
genCPs ex result (CPNoGuards p thenDo : ps) = do
  (cond, bind) <- isolate $ genPat ex p
  (thenEx, thenStmts) <- isolate $ genExpr thenDo
  elsePart <- genCPs ex result ps
  return $ If cond
             (Block $ bind ++ thenStmts ++ [Assign result thenEx])
             (Just elsePart)
genCPs ex result (CPGuards p cases : ps) = do
  error "fucking case patterns, how do they work"
  -- undefined
genCPs ex result [] = do
  return $ lsError "non-exhaustive pattern in function!"

-- Placeholder for "gen function something something blah blah" 
genDef = error "genDef: not implemented!"
