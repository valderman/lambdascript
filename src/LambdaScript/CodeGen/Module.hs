-- | Data structures for representing modules.
module LambdaScript.CodeGen.Module where
import LambdaScript.CodeGen.Ops
import LambdaScript.CodeGen.ShowJS
import Data.List (intercalate)

-- | Data type representing a single, exported function.
data Function = Function {
    funName :: String,
    args    :: [Var],
    stmts   :: [Stmt]
  }

instance Show Function where
  show f =
    "function " ++ funName f ++ "(" ++ arglist ++ ")\n" ++
    show (Block $ stmts f)
    where
      arglist = intercalate "," (map show (args f))
  showList (f:fs) =
    \x -> showList fs (show f) ++ x
  showList _ =
    id

-- | Data type representing a single module.
data Module = Module {
    modName :: String,
    exports :: [String],
    funcs   :: [Function]
  } deriving Show