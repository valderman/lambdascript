{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Type checker monad; provides facilities crucial to type checking and
--   inference.
module LambdaScript.TCM (
    TCM,
    runIn, runIn', run, blankEnv,
    pushScope, popScope,
    typeOf, declare, declaredInThisScope,
    getType, newType, mustBe,
    bind, typeOfVar
  ) where
import Prelude as P
import Data.Map as M
import Control.Monad.State as CMS
import LambdaScript.Abs as Abs
import LambdaScript.ErrM

-- | Internal state of the type checker.
data TCState = TCState {
    -- | Environment mapping from function names to their respective types.
    env         :: [Map String Type],
    -- | ID # of next type var to be generated.
    nextTypeVar :: Int,
    -- | Environment mapping from type variables to their respective types.
    --   Pushed and popped together with env.
    typeVars    :: [Map String Type],
    -- | Information about what constructors and type variables any given type
    --   has.
    typeStructs :: Map String ([String], [Constructor])
  } deriving Show

newtype TCM a = TCM {
    unTCM :: StateT TCState Err a
  } deriving (Monad, MonadState TCState)

-- | Run a type checking computation starting from the given state.
runIn :: TCM a -> TCState -> Err a
runIn (TCM m) st =
  runStateT m st >>= return . fst

-- | Run a type checking computation starting from the given state, returning
--   the state on computation finish.
runIn' :: TCM a -> TCState -> Err (a, TCState)
runIn' (TCM m) st =
  runStateT m st

-- | Run a type checking computation in a blank environment.
run :: TCM a -> Err a
run a =
  a `runIn` blankEnv

-- | Blank environment.
blankEnv :: TCState
blankEnv =
  TCState {
    env         = [empty],
    nextTypeVar = 0,
    typeVars    = [empty],
    typeStructs = empty
  }

-- | Push a new scope onto the scope stack. New symbols are always declared
--   in the top scope on the stack.
pushScope :: TCM ()
pushScope = do
  st <- get
  put st {env      = empty:env st,
          typeVars = empty:typeVars st}

-- | Remove the topmost scope from the scope stack.
popScope :: TCM ()
popScope =
  get >>= \st ->
      case env st of
        [_] -> fail "The top level scope can't be popped!"
        _   -> put st {env      = tail $ env st,
                       typeVars = tail $ typeVars st}

-- | Return the type of a symbol, failing if the symbol hasn't been declared.
--   If a symbol has been shadowed, the shadowing symbol is always returned,
--   and the original is not reachable until the shadowing symbol goes out of
--   scope.
typeOf :: String -> TCM Type
typeOf s = do
  st <- get
  case lookup' s (env st) of
    Just t -> return t
    _      -> fail $ "Symbol '" ++ s ++ "' hasn't been declared!"
  where
    lookup' s envs =
      foldl (\x env -> if x == Nothing
                         then M.lookup s env
                         else x)
            Nothing envs

-- | Declare a symbol to be of a given type. If the symbol was already
--   declared in this scope, its type is overwritten. If it was declared
--   in an outer scope, the old declaration is shadowed by the new one for the
--   lifetime of the current scope.
declare :: String -> Type -> TCM ()
declare s t = do
  st <- get
  let (e:envs) = env st
  put $ st {env = (insert s t e) : envs}

-- | Bind a type variable to a given concrete type. Has the same semantics as
--   declare.
bind :: String -> Type -> TCM ()
bind s t = do
  st <- get
  let (e:envs) = typeVars st
  put $ st {typeVars = (insert s t e) : envs}  

-- | Find out the type of the given type variable. 
typeOfVar :: String -> TCM (Maybe Type)
typeOfVar s = do
  st <- get
  return $ lookup' s $ typeVars st
  where
    lookup' s envs =
      foldl (\x env -> if x == Nothing
                         then M.lookup s env
                         else x)
            Nothing envs

-- | Check whether the given symbol was declared in the current scope or not.
--   A symbol with an unknown type is treated as though it hasn't been
--   declared.
declaredInThisScope :: String -> TCM Bool
declaredInThisScope id = do
  st <- get
  case M.lookup id (head $ env st) of
    Just x | x /= TUnknown -> return True
    _                      -> return False

-- | Get the concrete representation of the given type. Asking for a
--   nonexistent type gives an error.
getType :: String -> TCM ([String], [Constructor])
getType id = do
  st <- get
  case M.lookup id (typeStructs st) of
    Just t -> return t
    _      -> fail $ "Type '" ++ id ++ "' hasn't been declared!"

-- | Declare a new type. Types don't change at all during type checking, so
--   here trying to redeclare a type produces an error.
newType :: String -> ([String], [Constructor]) -> TCM ()
newType id t = do
  st <- get
  case M.lookup id (typeStructs st) of
    Nothing -> put st {typeStructs = insert id t (typeStructs st)}
    _       -> fail $ "Type '" ++ id ++ "' already declared!"

-- | Takes two types and compares them. If one is more general than the other
--   but still compatible, it is specialized to be equivalent to the other.
--   If they are not equal, we fail type checking with an appropriate error
--   message.
mustBe :: Type -> Type -> TCM (Type, Type)
-- A variable is always compatible with anything, as we don't have any type
-- classes.
mustBe a@(TVariable (VIdent id)) b = do
  at <- typeOfVar id
  case at of
    Just a' -> a' `mustBe` b
    _       -> do
      bind id b
      return (a, b)

-- We only want that type var logic in one place.
mustBe a b@(TVariable _) = do
  (b, a) <- b `mustBe` a
  return (a, b)

-- If the types are equal, or if either one is unknown, both types shall be
-- equal.
mustBe a b
  | a == b =        return (a, b)
  | a == TUnknown = return (b, b)
  | b == TUnknown = return (a, a)

-- For two parametrized types (S a1,a2...) and (T b1,b2...), they are
-- compatible iff S == T and for each (a, b) a `mustBe` b.  
mustBe (TParam t1 args1) (TParam t2 args2) | t1 == t2 = do
  args <- mapM (\(a, b) -> a `mustBe` b) (zip args1 args2)
  let (args1', args2') = unzip args 
  return (TParam t1 args1', TParam t2 args2')

-- Tuples are really just a special case of parametrized types.
mustBe (TTuple args1) (TTuple args2) | length args1 == length args2 = do
  args <- mapM (\(TypeC a, TypeC b) -> a `mustBe` b) (zip args1 args2)
  let (args1', args2') = unzip args
  return (TTuple $ P.map TypeC args1', TTuple $ P.map TypeC args2')

-- As are lists.
mustBe (TList a) (TList b) = do
  (a', b') <- a `mustBe` b
  return (TList a', TList b')

-- And functions.
mustBe a@(TFun arg1 res1) b@(TFun arg2 res2) = do
  (arg1', arg2') <- arg1 `mustBe` arg2
  (res1', res2') <- res1 `mustBe` res2
  return (TFun arg1' res1', TFun arg2' res2')

-- If nothing else matches, the types are not compatible.
mustBe a b =
  fail $ "Incompatible types: '" ++ show a ++ "' and '" ++ show b ++ "'"
