-- | Default type definitions and declarations for LS
module LambdaScript.TypeDefaults (assumptions) where
import Prelude hiding (maybe, either)
import LambdaScript.Types
import LambdaScript.Abs

assumptions :: [Assump]
assumptions = map (\(id, t) -> id :>: quantify theVars t) builtins

theVars = map VIdent ["a"]

builtins :: [(ID, Type)]
builtins = [
    ("()",    tUnit),
    ("error", tString ~> tv "a")
  ]
