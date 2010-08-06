{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Type checker monad; provides facilities crucial to type checking and
--   inference.
module LambdaScript.TCM (
    TCM,
    runIn, runIn', run, blankEnv,
    pushScope, popScope,
    typeOf, declare, declaredInThisScope
  ) where
import Data.Map as M
import Control.Monad.State as CMS
import LambdaScript.Abs as Abs
import LambdaScript.ErrM

-- | State of the type checker; consists of a stack of environments.
--   This is needed because we want to be able to shadow symbols declared in
--   outer scopes while still being able to error out if the user tries to
--   declare the same symbol twice in the same scope.
newtype TCState = TCState {
    env :: [Map String Type]
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
  TCState [empty]

-- | Push a new scope onto the scope stack. New symbols are always declared
--   in the top scope on the stack.
pushScope :: TCM ()
pushScope =
  get >>= \(TCState envs) -> put $ TCState $ empty:envs

-- | Remove the topmost scope from the scope stack.
popScope :: TCM ()
popScope =
  get >>= \(TCState envs) ->
      case envs of
        [_] -> fail "The top level scope can't be popped!"
        _   -> put $ TCState $ envs

-- | Return the type of a symbol, failing if the symbol hasn't been declared.
--   If a symbol has been shadowed, the shadowing symbol is always returned,
--   and the original is not reachable until the shadowing symbol goes out of
--   scope.
typeOf :: String -> TCM Type
typeOf s = do
  TCState envs <- get
  case lookup' s envs of
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
  TCState (env:envs) <- get
  put $ TCState $ (insert s t env) : envs

-- | Check whether the given symbol was declared in the current scope or not.
--   A symbol with an unknown type is treated as though it hasn't been
--   declared.
declaredInThisScope :: String -> TCM Bool
declaredInThisScope id = do
  TCState (env:_) <- get
  case M.lookup id env of
    Just x | x /= TUnknown -> return True
    _                      -> return False