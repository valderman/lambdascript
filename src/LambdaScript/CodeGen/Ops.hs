{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Data structures for representing operations and data during intermediate
--   code generation.
module LambdaScript.CodeGen.Ops where
import LambdaScript.Abs (Type)

data Exp
  = Thunk     Exp
  | Eval      Exp
  | Tailcall  Exp
    -- Use the second expression as an array index into the first
  | Index     Exp Exp
  | Array     [Exp]
    -- Evaluate the expression and compare its constructor to the given
    -- constructor ID. If they're the same return true, else return false.
  | ConstrIs  Exp ConstrID
  | Cons      Exp Exp
  | Const     Const
  | Ident     Var
  | Oper      Oper Exp Exp
  | Neg       Exp
  | FunExp    Fun
  | IfExp     Exp Exp Exp -- ternary operator
  | Call      Exp [Exp]
  deriving Show

data Stmt
  = Assign Var Exp
  | If     Exp Stmt (Maybe Stmt)
  | Return Exp
  | Block  [Stmt]
  deriving Show

-- | Operators
data Oper
  = Add | Sub | Mul | Div | Mod
  | And | Or
  | Eq  | Lt  | Gt  | Le  | Ge | Ne
  deriving Show

-- | Functions
data Fun
  = FunIdent String
  | Lambda    [Var] Stmt
  | Construct Type  ConstrID
  deriving Show

type NamedFunction = (String, [Var], Stmt)

-- | Constants
data Const
  = NumConst  Double -- All numbers are doubles in ECMAScript
  | CharConst Char
  | StrConst  String
  | BoolConst Bool
  | EmptyListConst
  deriving Show

strConst :: String -> Const
strConst = StrConst

-- | Representation of a variable; basically just an ID.
newtype Var = Var {unV :: Int} deriving (Eq, Num, Show)

-- | A constructor ID
newtype ConstrID = ConstrID Int deriving (Eq, Num, Show)
