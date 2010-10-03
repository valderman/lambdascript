{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Data structures for representing operations and data during intermediate
--   code generation.
module LambdaScript.CodeGen.Ops where

data Exp
  = Thunk     Exp
  | Tailcall  Exp
  | Construct ConstrID [Exp]
    -- Evaluate the expression and compare its constructor to the given
    -- constructor ID. If they're the same return true, else return false.
  | ConstrIs  Exp ConstrID
  | Const     Const
  | Ident     Var
  | Oper      Exp Exp
  | Neg       Exp
  | IfExp     Exp Exp Exp -- ternary operator
  | Call      Fun [Exp]

data Stmt
  = Assign Var Exp
  | If     Exp Stmt (Maybe Stmt)
  | Return Exp

-- | Operators
data Oper
  = Add | Sub | Mul | Div | Mod
  | And | Or
  | Eq  | Lt  | Gt  | Le  | Ge | Ne

-- | Functions
data Fun
  = FunIdent String
  | Lambda   [Var] [Stmt]

type NamedFunction = (String, [Var], [Stmt])

-- | Constants
data Const
  = NumConst  Double -- All numbers are doubles in ECMAScript
  | CharConst Char
  | StrConst  String
  | BoolConst Bool

-- | Representation of a variable; basically just an ID.
newtype Var = Var {unV :: Int} deriving (Eq, Num, Show)

-- | A constructor ID
newtype ConstrID = ConstrID Int deriving (Eq, Num, Show)
