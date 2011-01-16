-- | Data structures for representing modules.
module LambdaScript.CodeGen.Module where
import LambdaScript.CodeGen.Ops
import LambdaScript.CodeGen.ShowJS
import LambdaScript.Abs (Type (..))
import Data.List (intercalate)

-- | Data type representing a single, exported function.
data Function = Function {
    funName :: String,
    args    :: [Var],
    stmts   :: [Stmt],
    funType :: Type
  }

instance Show Function where
  show f =
    "$._" ++ funName f ++ " = function(" ++ arglist ++ ")" ++
    show (Block $ stmts f) ++ ";\n"
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
  }

instance Show Module where
  show (Module name exports funcs) =
    "function _" ++ name ++ "(){\nvar $ = this;\n" ++
        concat (map show funcs) ++
        concat (map export exports) ++
      "}\n" ++ name ++ " = new _" ++ name ++ "();\n"
    where
      export f =
        "$['" ++ f ++ "'] = _exp($._" ++ f ++ ");\n"
