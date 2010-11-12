-- | Data structures for representing modules.
module LambdaScript.CodeGen.Module where
import LambdaScript.CodeGen.Ops

-- | Data type representing a single, exported function.
data Function = Function {
    funName :: String,
    mod     :: String,
    stmts   :: [Stmt]
  } deriving Show

-- | Data type representing a single module.
data Module = Module {
    modName :: String,
    funcs   :: [Function]
  } deriving Show