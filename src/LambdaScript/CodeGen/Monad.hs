-- | Code generation monad for LS. We start by generating SSA form intermediate
--   code, which may then be either dumped to file or (preferrably) run
--   through some optimization passes.
module LambdaScript.CodeGen.Monad where
import Prelude hiding (mod)
import Control.Monad.State as CMS
import Data.Map as M
import LambdaScript.CodeGen.Module
import LambdaScript.CodeGen.Ops
import LambdaScript.Abs as Abs (Expr, ConstDef (..), Ident (..))

data CGState = CGState {
    code         :: [Stmt],
    env          :: Map String Var,
    constructors :: Map String ConstrID,
    nextID       :: Var
  }

type CG = State CGState

-- | Create a blank starting state, using the given mapping of constructor IDs.
blankState :: Map String ConstrID -> CGState
blankState cs = CGState {
    code         = [],
    env          = M.empty,
    constructors = cs,
    nextID       = 0
  }

-- | Emit a statement.
stmt :: Stmt -> CG ()
stmt s = do
  st <- get
  put st {code = s:code st}

-- | Create a new variable ID.
newVar :: CG Var
newVar = do
  st <- get
  let id = nextID st
  put st {nextID = id + 1}
  return id

-- | Bind the given identifier to the given ID.
bind :: String -> Var -> CG ()
bind id var = do
  st <- get
  put st {env = insert id var $ env st}

-- | Get the ID of the given variable.
idOf :: String -> CG Var
idOf s = do
  st <- get
  case M.lookup s (env st) of
    Just id -> return id
    _       -> fail $ "Internal error: idOf " ++ s ++ " failed!"

-- | Get the ID of the given constructor.
constrID :: String -> CG ConstrID 
constrID s = do
  st <- get
  case M.lookup s (constructors st) of
    Just id -> return id
    _       -> fail $ "Internal error: constrID " ++ s ++ " failed!"

-- | Run the given computation using the current environment, returning any
--   emitted statements after the run, and discarding those statements from
--   the environment.
isolate :: CG a -> CG (a, [Stmt])
isolate m = do
  st <- get
  case runState m st of
    (a, st') -> do
      put st' {code = code st}
      return (a, reverse $ code st')

-- | Generate code for a function, using the given generator.
gen :: (Expr -> CG ())     -- ^ Generator to use for code generation.
    -> Map String ConstrID -- ^ Mapping of constructor names to IDs.
    -> ConstDef            -- ^ The definition to generate code for.
    -> Function            -- ^ The resulting Function.
gen m cs (ConstDef (Abs.Ident id) ex) =
  case runState (m ex) (blankState cs) of
    (_, st) -> Function {
        funName = id,
        mod     = "",
        stmts   = reverse $ code st
      }