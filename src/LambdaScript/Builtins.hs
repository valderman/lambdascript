-- | Definitions for all built-in types and functions.
module LambdaScript.Builtins where
import LambdaScript.Abs
import LambdaScript.Types

assumptions :: [Assump]
assumptions = map (\(id, t) -> id :>: quantify theVars t) defs

functions :: Def
functions =
  BGroup $ BindGroup $ flip map defs $ \(name, t) ->
    ConstDef (Ident name) (ETyped (EConstr $ TIdent "()") t)

theVars = map VIdent ["a"]

defs :: [(ID, Type)]
defs = [
    ("()",    tUnit),
    ("error", tString ~> tv "a"),
    ("_jsfun", tString ~> tInt ~> tv "a")
  ]

types :: [NewType]
types = [
    NewType (TIdent "Bool") [] [
      Constructor (TIdent "False") [],
      Constructor (TIdent "True") []
    ]
  ]