module LambdaScript.TypeChecker (
    annotate
  ) where
import Control.Monad
import LambdaScript.TCM as TCM
import LambdaScript.Abs as Abs

-- | Type check the program and annotate all expressions and pattern matches.
annotate :: Program -> TCM Program
annotate p@(Program defs) = do
  addTypes p
  p <- addSignatures p
  p <- mapM annotateDef defs
  return $ Program p

-- | Give the type checker knowledge of the structure of all user-defined
--   types.
addTypes :: Program -> TCM ()
addTypes p@(Program defs) = do
  forM_ defs addType
  where
    addType (TypeDef (NewType (TIdent id) params constrs)) = do
      mapM_ (declareConstructor id params) constrs
      newType id (map (\(AnyVar (VIdent x)) -> x) params, constrs)
    addType _ =
      return ()

    -- Calculate the type of the constructor, then declare it.
    declareConstructor tid leftargs (Constructor (TIdent cid) args) = do
      let leftargs' = map (\(AnyVar x) -> TVariable x) leftargs
      let args' = map (\(TypeEmpty x) -> x) args
      let fType = foldl (flip TFun) (TParam (TIdent tid) leftargs') args'
      declare cid fType

-- | Return the number of arguments a function of the given type takes.
numArgs :: Type -> Int
numArgs (TFun _ f) = numArgs f + 1
numArgs _          = 0

-- | Returns the type of a function's arguments.
getArgTypes :: Type -> [Type]
getArgTypes (TFun t next) = t:getArgTypes next
getArgTypes _             = []

-- | Returns the final result type of a function type.
--   For example, getResultType (Int -> Int -> Int) == Int.
getResultType :: Type -> Type
getResultType (TFun _ next) = getResultType next
getResultType t             = t

-- | Type check and annotate a function definition
annotateDef :: Def -> TCM Def
annotateDef (FunDef (VIdent id) (TPNoGuards pats expr)) = do
  pushScope
  pats' <- annotatePatterns id pats
  popScope
  return $ FunDef (VIdent id) (TPNoGuards pats' expr)
annotateDef (FunDef (VIdent id) (TPGuards pats guardeds)) = do
  pushScope
  pats' <- annotatePatterns id pats
  (guards, exprs) <- annotateGuards $ unzip
                                    $ map (\(GuardedTopExpr g e) -> (g, e))
                                    $ guardeds
  popScope
  return $ FunDef (VIdent id) (TPGuards pats' guardeds)
annotateDef x =
  return x

annotateGuards = return . id

-- | Annotate all patterns for the function with the given name.
annotatePatterns :: String -> [Pattern] -> TCM [Pattern]
annotatePatterns id pats = do
  fType <- typeOf id
  let argTypes = if fType /= TUnknown
                    then getArgTypes fType
                    else replicate (length pats) TUnknown
  if length pats > length argTypes
     then fail $ "Too many arguments to function '" ++ id ++ "'!"
     else return ()
  mapM (uncurry annotatePattern) (zip argTypes pats)

-- | Type check and annotate a pattern.
annotatePattern :: Type -> Pattern -> TCM Pattern
-- For int constants, just check that there are no conflicting type signatures.
annotatePattern t p@(PInt _) =
  t `unify` (TSimple $ TIdent "Int") >>= return . PTyped p

-- Same as for ints.
annotatePattern t p@(PDoub _) =
  t `unify` (TSimple $ TIdent "Double") >>= return . PTyped p

-- Same as for ints.
annotatePattern t p@(PString _) =
  t `unify` (TList $ TSimple $ TIdent "Char") >>= return . PTyped p

-- Same as for ints.
annotatePattern t p@(PChar _) =
  t `unify` (TSimple $ TIdent "Char") >>= return . PTyped p

-- An identifier; just try to infer its type.
annotatePattern t p@(PID _) =
  t `unify` TUnknown >>= return . PTyped p

-- A list of something; make sure a list is OK here and try to infer the type
-- of the list as well, then make sure that all inside patterns have the same
-- type, and use that type for the type of the final list.
annotatePattern t p@(PList pats) = do
  (TList t') <- t `unify` TList TUnknown

  -- Check that all items are internally type correct
  pats' <- mapM (annotatePattern t') (map (\(PatC x) -> x) pats)

  -- Check that all list items have the same type as the first
  let (PTyped _ t'') = if not $ null pats'
                          then head pats'
                          else PTyped PWildcard TUnknown
  p' <- mapM (\(PTyped p t2) -> t2 `unify` t'' >>= return . PTyped p)
             pats'
  return $ PTyped (PList (map PatC p')) (TList t'')

-- Data constructor; look up the structure of the constructor's type and check
-- the constructor's args, if any, against that.
annotatePattern t p@(PConstr (TIdent id) pats) = do
  t' <- typeOf id
  rt <- getResultType t' `unify` t
  let args = getArgTypes t'
  if length pats /= length args
     then fail $  "Constructor '" ++ id ++ "' has the wrong number of "
               ++ "arguments in pattern match!"
     else return ()
  pats' <- mapM (uncurry annotatePattern) (zip args pats)
  
  -- Make sure to resolve any unknown type variables.
  rt <- rt `unify` rt

  -- As we demand the right # of args, the type will always be the final
  -- return type.
  return $ PTyped (PConstr (TIdent id) pats') rt

-- Tuple; infer all elements, then make sure they fit any type signatures.
annotatePattern t p@(PTuple pats) = do
  -- Apply type signature types and check that this is indeed a tuple
  (TTuple t') <- t `unify` (TTuple $ replicate (length pats) $ TypeC TUnknown)
  
  -- Check elements of tuple
  let t'' = (map (\(TypeC t) -> t) t')
  let pats' = map (\(PatC p) -> p) pats
  pats' <- mapM (uncurry annotatePattern) (zip t'' pats') 
  let tFinal = map (\(PTyped _ t) -> TypeC t) pats'
  return $ PTyped (PTuple $ map PatC pats') (TTuple tFinal)

-- Cons list; nothing weird here.
annotatePattern t p@(PCons x xs) = do
  -- Is there a type signature to tell us anything?
  (TList t') <- t `unify` TList TUnknown
  x' <- annotatePattern t' x

  -- Did we learn anything new from annotating the element?
  let PTyped x'' t'' = x'
  xs' <- annotatePattern (TList t'') xs

  -- Did we learn anything new from annotating the list?
  let PTyped xs'' tl' = xs'

  -- Make sure list and element agree
  tlist@(TList telem) <- TList t'' `unify` tl'
  return $ PTyped (PCons (PTyped x'' telem) (PTyped xs'' tlist)) tlist

-- Wildcard matches anything, can have any type.
annotatePattern t PWildcard =
  t `unify` TUnknown >>= return . PTyped PWildcard

-- Error trap for debugging
annotatePattern _ p = do
  error $ "Unhandled pattern match: " ++ show p


-- | Make a pass through all top level functions, adding type signatures where
--   explicit.
addSignatures :: Program -> TCM Program
addSignatures p@(Program defs) = do
  forM_ defs addFunc
  forM_ defs addSig
  return p
  where
    -- Add all top level functions to environment, using unknown type.
    addFunc (FunDef (VIdent id) _) =
      id `declare` TUnknown
    addFunc _ =
      return ()

    -- Add types to all top level functions with explicit signatures. Error
    -- out if someone tries to put a type on a nonexistent function.
    addSig (TypeDecl (VIdent id) t) = do
      tOld <- typeOf id
      case tOld of
        TUnknown -> id `declare` t
        _        -> fail $ "Symbol '" ++ id ++ "' already has a type signature"
    addSig _ =
      return ()
