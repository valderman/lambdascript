-- | Definitions for all built-in types and functions.
module LambdaScript.Builtins where
import LambdaScript.Abs
import LambdaScript.Types

assumptions :: [Assump]
assumptions = map (\(id, t) -> id :>: quantify theVars t) defs

functions :: Def
functions =
  BGroup $ BindGroup $ flip map defs $ \(name, _) ->
    ConstDef (Ident name) (EConstr $ TIdent "()")

theVars = map VIdent ["a"]

defs :: [(ID, Type)]
defs = [
    ("()",        tUnit),
    ("error",     tString ~> tv "a"),
    ("undefined", tv "a"),
    ("otherwise", tBool)
  ]

types :: [NewType]
types = [
    NewType (TIdent "Bool") [] [
      Constructor (TIdent "True") [],
      Constructor (TIdent "False") []
    ],
    NewType (TIdent "Maybe") [AnyVar $ VIdent "a"] [
      Constructor (TIdent "Just") [TypeEmpty $ tv "a"],
      Constructor (TIdent "Nothing") []
    ]
  ]