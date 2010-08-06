{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Type checker monad; provides facilities crucial to type checking and
--   inference.
module LambdaScript.TCM (
    TCM,
    runIn, runIn', run, blankEnv,
    pushScope, popScope,
    typeOf, declare, declaredInThisScope,
    getType, newType
  ) where
import Data.Map as M
import Control.Monad.State as CMS
import LambdaScript.Abs as Abs
import LambdaScript.ErrM

-- | State of the type checker; consists of a stack of environments.
--   This is needed because we want to be able to shadow symbols declared in
--   outer scopes while still being able to error out if the user tries to
--   declare the same symbol twice in the same scope.
--   The type checker state also includes a mapping from type names to their
--   actual structure, so we know what (TIdent "Tree") means in concrete terms.
data TCState = TCState {
    env   :: [Map String Type],
    types :: Map String Type
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
    env   = [empty],
    types = empty
  }

-- | Push a new scope onto the scope stack. New symbols are always declared
--   in the top scope on the stack.
pushScope :: TCM ()
pushScope =
  get >>= \st -> put st {env = empty:env st}

-- | Remove the topmost scope from the scope stack.
popScope :: TCM ()
popScope =
  get >>= \st ->
      case env st of
        [_] -> fail "The top level scope can't be popped!"
        _   -> put st {env = tail $ env st}

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
getType :: String -> TCM Type
getType id = do
  st <- get
  case M.lookup id (types st) of
    Just t -> return t
    _      -> fail $ "Type '" ++ id ++ "' hasn't been declared!"

-- | Declare a new type. Types don't change at all during type checking, so
--   here trying to redeclare a type produces an error.
newType :: String -> Type -> TCM ()
newType id t = do
  st <- get
  case M.lookup id (types st) of
    Nothing -> put st {types = insert id t (types st)}
    _       -> fail $ "Type '" ++ id ++ "' already declared!"
