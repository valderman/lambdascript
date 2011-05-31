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

instance ShowJS Function where
  showJS c f =
    "$._" ++ funName f ++ " = function(" ++ arglist ++ ")" ++
    showJS c (Block $ stmts f) ++ ";\n"
    where
      arglist = intercalate "," (map (showJS c) (args f))

-- | Data type representing a single module.
data Module = Module {
    modName :: String,
    exports :: [String],
    funcs   :: [Function]
  }

instance ShowJS Module where
  showJS c (Module name exports funcs) =
    "function _" ++ name ++ "(){\nvar $ = this;\n" ++
        concat (map (showJS c) funcs) ++
        concat (map export exports) ++
      "}\n" ++ name ++ " = new _" ++ name ++ "();\n"
    where
      export f =
        "$['" ++ f ++ "'] = _exp($._" ++ f ++ ");\n"
