module LambdaScript.CodeGen.Generate (function, generate) where
import LambdaScript.Abs
import LambdaScript.CodeGen.Monad
import LambdaScript.CodeGen.Ops as Ops
import LambdaScript.CodeGen.Errors
import LambdaScript.CodeGen.Module
import qualified Data.Map as M
import Data.List (foldl1')
import Control.Monad (foldM)

generate :: M.Map String ConstrID -> Program -> [Function]
generate constrs (Program defs) =
  concat $ map (\(BGroup d) -> bindgroup constrs d) defs

bindgroup :: M.Map String ConstrID -> BindGroup -> [Function]
bindgroup constrs (BindGroup defs) =
  map (gen function constrs) defs

function :: Expr -> CG ()
function ex = genExpr ex >>= stmt . Return

eval :: Exp -> Exp
eval ex@(Thunk _) = Eval ex
eval ex           = ex

thunk :: Exp -> Exp
thunk ex@(Thunk _) = ex
thunk ex           = Thunk ex

oper :: Oper -> Expr -> Expr -> CG Exp
oper op a b = do
  a' <- genExpr a
  b' <- genExpr b
  return . thunk $ Oper op (eval a') (eval b')

allTrue :: [Exp] -> Exp
allTrue = foldl1' (Oper And)

genExpr :: Expr -> CG Exp
genExpr (ETyped ex t) = genExpr' t ex
  where
    genExpr' t (EConstr (TIdent id)) = do
      constrID id >>= return . FunExp . Construct t
    genExpr' t (EVar (VIdent id)) = do
      idOf id >>= return . Ops.Ident

    genExpr' t (EInt n) = do
      return . Ops.Const . NumConst . fromIntegral $ n
    genExpr' t (EDoub d) = do
      return . Ops.Const . NumConst $ d
    genExpr' t (EChar c) = do
      return . Ops.Const . CharConst $ c
    genExpr' t (EStr s) = do
      return . Ops.Const . strConst $ s
    
    genExpr' t (EApp f x) = do
      f' <- genExpr f
      x' <- genExpr x
      return . thunk $ Call (eval f') [x']

    genExpr' t (EList (ex:exs)) = do
      ex' <- genExpr ex
      exs' <- genExpr (EList exs)
      return . thunk $ Cons ex' exs'
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

    genExpr' t (ELambda ps ex) = do
      return . thunk . FunExp =<< genLambda ps ex
      where
        genLambda ps ex = do
          (args, stmts) <- isolate $ do
            vars <- mapM (\_ -> newVar) ps
            (exps, binds) <- isolate $ sequence $ zipWith genPat (map Ops.Ident vars) ps
            (body, stmts) <- isolate $ genExpr ex
            stmt $ If (allTrue exps)
                      (Block $ binds ++ stmts ++ [Return body])
                      (Just lambdaPatternMismatch)
            return vars
          return $ Lambda args (Block stmts)

    genExpr' t (EIf cond ifE elE) = do
      cond' <- genExpr cond
      ifE' <- genExpr ifE
      elE' <- genExpr elE
      return . thunk $ IfExp (eval cond') ifE' elE'

    genExpr' t (ECase ex cps) = do
      ex' <- genExpr ex
      v <- newVar
      genCPs ex' v cps >>= stmt
      return $ Ops.Ident v

    genExpr' t (EBinds ex defs) = do
      mapM_ genDef defs
      genExpr ex

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
  cond <- genPat ex p
  (thenEx, thenStmts) <- isolate $ genExpr thenDo
  elsePart <- genCPs ex result ps
  return $ If cond
             (Block $ thenStmts ++ [Assign result thenEx])
             (Just elsePart)
genCPs ex result (CPGuards p cases : ps) = do
  error "fucking case patterns, how do they work"
  -- undefined
genCPs ex result [] = do
  return $ lsError "non-exhaustive pattern in function!"

genDef = error "genDef lolwut"
