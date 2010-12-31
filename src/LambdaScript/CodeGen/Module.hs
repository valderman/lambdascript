-- | Data structures for representing modules.
module LambdaScript.CodeGen.Module where
import LambdaScript.CodeGen.Ops
import LambdaScript.CodeGen.ShowJS

-- | Data type representing a single, exported function.
data Function = Function {
    funName :: String,
    mod     :: String,
    stmts   :: [Stmt]
  }

instance Show Function where
  show f =
    "function " ++ funName f ++ "(){\n" ++
    show (Block $ stmts f) ++
    "}\n"

-- | Data type representing a single module.
data Module = Module {
    modName :: String,
    funcs   :: [Function]
  } deriving Show