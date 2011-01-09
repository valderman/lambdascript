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

-- | Generate code for all functions in a program. Takes a list of imports and
--   a program to generate code for as its arguments.
generate :: [(String, String)] -> Program -> [Function]
generate imports p@(Program defs) =
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
            map (gen function constrs env 0) d
          _        ->
            []

    -- Assign IDs for every definition in every bind group.
    env =
      case concat $ map numberGroups (Builtin.functions : defs) of
        e -> M.fromList $ e ++ map (\(m,n) -> (n, Ops.Import m n)) imports

    -- Assign IDs to all definitions in a single bind group.
    numberGroups (BGroup (BindGroup defs)) =
      map (\(ConstDef (A.Ident id) ex) -> (id, Global (nargs ex) id)) defs
    numberGroups _ =
      []

-- | Code generator for a single function.
function :: Expr -> CG ()
function ex = genExpr ex >>= stmt . Return (nargs ex)

-- | Smack an Eval primitive on an expression.
eval :: Exp -> Exp
eval ex = Eval ex

-- | Turn an expression into a thunk, without double-thunking it.
thunk :: Exp -> Exp
thunk (Eval ex)    = ex
thunk ex@(Thunk _) = ex
thunk ex           = Thunk ex

-- | Generate lazy code for a binary operator.
oper :: Oper -> Expr -> Expr -> CG Exp
oper op a b = do
  a' <- genExpr a
  b' <- genExpr b
  return $ Oper op a' b'

-- | Create a conjunction of a bunch of expressions.
allTrue :: [Exp] -> Exp
allTrue = foldl1' (Oper And)

-- | Returns the number of arguments the given expression would take when
--   called as a function.
nargs :: Expr -> Int
nargs (ETyped _ t) = nargs' t
  where
    nargs' (TOp _ t) = 1+nargs' t
    nargs' _         = 0

genExpr :: Expr -> CG Exp
genExpr (ETyped ex t) = genExpr' t ex
  where
    -- True/False constructors are special
    genExpr' t (EConstr (TIdent "True")) = do
      return $ Ops.Const $ BoolConst  True
    genExpr' t (EConstr (TIdent "False")) = do
      return $ Ops.Const $ BoolConst False
    
    -- Constructor; just look it up.
    genExpr' t (EConstr (TIdent id)) = do
      constrID id >>= return . FunExp . Construct t

    -- A symbol; all symbols are thunks, so we must evaluate it.
    genExpr' t (EVar (VIdent id)) = do
      idOf id >>= return . eval . Ops.Ident

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
    -- the argument, then apply the function to the thunked argument.
    genExpr' t (EApp f x) = do
      f' <- genExpr f
      x' <- genExpr x
      return $ Call (nargs f-1) f' [thunk x']

    -- Various combinators for expressions.
    genExpr' t (EList (ex:exs)) = do
      ex' <- genExpr ex
      -- The entire list has the same type; use genExpr' instead of genExpr
      -- since EList exs is untyped and genExpr expects ETyped ...
      exs' <- genExpr' t (EList exs)
      -- Cons is a function application, so thunk the arguments.
      return $ Cons (thunk ex') (thunk exs')
    genExpr' t (EList []) =
      return . Ops.Const $ EmptyListConst

    genExpr' t (ETuple elems) = do
      elems' <- mapM genExpr elems
      -- A tuple is a constructor, so thunk the arguments!
      return $ Array $ map thunk elems'
    genExpr' t (ECons x xs) = do
      x' <- genExpr x
      xs' <- genExpr xs
      -- Again, Cons is a function so thunk the args.
      return $ Cons (thunk x') (thunk xs')
    genExpr' t (ENot ex) = do
      ex' <- genExpr ex
      return $ Neg ex'
    genExpr' t (EConcat a b) = do
      a' <- genExpr a
      b' <- genExpr b
      -- ++ is also a function, so thunk the args.
      return $ Call 0 (FunExp $ FunIdent "_conc") [thunk a', thunk b']
    
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
      return . FunExp =<< genLambda ps ex
      where
        genLambda ps ex = do
          (args, stmts) <- isolate $ do
            -- Create a var to refer to each argument
            vars <- mapM (\_ -> newVar) ps
            -- Generate pattern matching and binding code
            (exps, binds) <- isolate $ sequence $ zipWith genPat (map (eval . Ops.Ident) vars) ps
            -- Generate code for the lambda body
            (body, stmts) <- isolate $ genExpr ex
            -- If everything matches, we execute the body. Otherwise we
            -- runtimefail.
            stmt $ If (allTrue exps)
                      (Block $ binds ++ stmts ++ [Return (nargs ex) body])
                      (Just lambdaPatternMismatch)
            return vars
          return $ Lambda args (Block stmts)

    -- If expression; if(a) {v = thenEx;} else {v = elseEx;}
    genExpr' t (EIf cond ifE elE) = do
      v <- newVar
      cond' <- genExpr cond
      (ifE', ifS) <- isolate $ genExpr ifE
      (elE', elS) <- isolate $ genExpr elE
      stmt $ If cond'
                (Block $ ifS ++ [Assign v $ thunk ifE'])
                (Just . Block $ elS ++ [Assign v $ thunk elE'])
      return $ eval $ Ops.Ident v

    -- Case expression; works pretty much like a lambda with an arbitrary
    -- number of bodies. (See also: recursive documentation)
    genExpr' t (ECase ex cps) = do
      ex' <- genExpr ex
      v <- newVar
      genCPs ex' v cps >>= stmt . Forever
      return $ eval $ Ops.Ident v

    -- Bindings; we just generate them and assign them to local vars.
    genExpr' t (EBinds ex defs) = do
      -- Generate vars to bind each function to
      binds <- sequence [newVar >>= \v -> bind id v >> return (v, id, ex)
                        | BGroup (BindGroup group) <- defs,
                          ConstDef (A.Ident id) ex <- group]
      -- Generate code for each function
      mapM_ genDef binds
      -- Finally generate code for the expression.
      genExpr ex
      where
        genDef (v, id, ex) = do
          ((), stmts) <- isolate $ function ex
          stmt $ Assign v (FunExp $ Lambda [] (Block stmts))
    
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
  -- Don't thunk the assignment if ex came from indexing an array; if it did,
  -- the expression is a constructor field, which is guaranteed to already be
  -- a thunk.
  case ex of
    Index _ _ -> stmt $ Assign v ex
    _         -> stmt $ Assign v $ thunk ex
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
-- True/False constructors are special
genPat ex (PConstr (TIdent "True") _) = do
  return $ Oper Eq ex (Ops.Const $ BoolConst True)
genPat ex (PConstr (TIdent "False") _) = do
  return $ Oper Eq ex (Ops.Const $ BoolConst False)
genPat ex (PConstr (TIdent id) args) = do
  cid <- constrID id
  (_, cond) <- foldM (\(n, cond) p -> do
                         c <- genPat (eval $ Index ex $ Ops.Const $ NumConst n) p
                         return (n+1, Oper And cond c))
                     (1, (ex `ConstrIs` cid))
                     args
  return cond
genPat _ PWildcard = do
  return $ Ops.Const $ BoolConst True
genPat ex (PCons hp tp) = do
  let isCons = Index ex $ Ops.Const $ NumConst 0
  let h = eval $ Index ex $ Ops.Const $ NumConst 1
  let t = eval $ Index ex $ Ops.Const $ NumConst 2
  h' <- genPat h hp
  t' <- genPat t tp
  -- This relies on [a] being defined as data [a] = [] | a : [a]
  -- That is, [] has ID 0 and : has ID 1.
  return $ Oper And isCons (Oper And h' t')

-- | Generate code for a tuple pattern match. Tuples are represented as JS
--   arrays, so to generate code to match them we just recurse through the
--   list of patterns, increasing an index each time, and AND together the
--   returned conditions. Bindings are emitted by genPat as usual.
genPTuple :: Int -> Exp -> [PatC] -> CG Exp
genPTuple n ex (PatC p : pats) = do
  let x = eval $ Index ex (Ops.Const $ NumConst $ fromIntegral n)
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
  let isCons = Index ex $ Ops.Const $ NumConst 0
  let h = eval $ Index ex $ Ops.Const $ NumConst 1
  let t = eval $ Index ex $ Ops.Const $ NumConst 2  
  h' <- genPat h p
  t' <- genPList t pats
  -- This relies on [a] being defined as data [a] = [] | a : [a]
  -- That is, [] has ID 0 and : has ID 1.
  return $ Oper And isCons (Oper And h' t')
genPList ex [] =
  -- ...as does this.
  return $ Ops.Neg (Index ex $ Ops.Const $ NumConst 0)

-- | Generate a set of case patterns. A case pattern is a pattern of the form
--   pattern [| guard] -> expression. The given Exp gives the variable that is
--   to be matched against the pattern, and the Var is the variable where the
--   result of the case expression will be placed.
genCPs :: Exp -> Var -> [CasePattern] -> CG Stmt
genCPs ex result (CPNoGuards p thenDo : ps) = do
  (cond, bind) <- isolate $ genPat ex p
  (thenEx, thenStmts) <- isolate $ genExpr thenDo
  (Block elsePart) <- genCPs ex result ps
  -- We don't use the else part, because we need fall-through behaviour
  -- when we introduce guards into the mix. For example:
  -- foo (Just x) | x < 2 = bar
  -- foo (Just 7)         = blah
  -- The naive way to compile this would be something like
  -- if isJust arg:
  --   if x < 2:
  --     bar
  -- else if x == 7: 
  --   blah
  -- else:
  --   error
  -- However, if isJust arg and not x < 2, there's no way to reach x == 7.
  -- Instead, we put everything inside a loop and then do:
  -- if isJust arg:
  --   if x < 2:
  --     bar
  --     break
  -- if x == 7: 
  --   blah
  --   break
  -- else:
  --   error
  -- ...which gives us the desired fall-through behaviour.
  return . Block $ (If cond
                       (Block $  bind
                              ++ thenStmts
                              ++ [Assign result $ thunk thenEx, Break])
                       Nothing) : elsePart
genCPs ex result (CPGuards p cases : ps) = do
  (cond, bind) <- isolate $ genPat ex p
  gs <- mapM (genGuard result) cases
  (Block elsePart) <- genCPs ex result ps
  return . Block $ (If cond
                       (Block $ bind ++ gs)
                       Nothing) : elsePart
genCPs ex result [] = do
  return $ Block [lsError "non-exhaustive pattern in function!"]

-- | Generate a guarded expression. Basically, if the guard holds we execute
--   the associated expression and assign its return value to the result var,
--   otherwise do nothing.
genGuard :: Var -> GuardedCaseExpr -> CG Stmt
genGuard v (GuardedCaseExpr (GuardExpr ge) ex) = do
  guard <- genExpr ge
  (expr, stmts) <- isolate $ genExpr ex
  return $ If guard
              (Block $ stmts ++ [Assign v $ thunk expr, Break])
              Nothing
