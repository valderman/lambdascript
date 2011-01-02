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
  | Call      Exp [Exp]

data Stmt
  = Assign Var Exp
  | If      Exp Stmt (Maybe Stmt)
  | Return  Exp
  | Block   [Stmt]
  | Forever Stmt
  | Break

-- | Operators
data Oper
  = Add | Sub | Mul | Div | Mod
  | And | Or
  | Eq  | Lt  | Gt  | Le  | Ge | Ne

-- | Functions
data Fun
  = FunIdent String
  | Lambda    [Var] Stmt
  | Construct Type  ConstrID

type NamedFunction = (String, [Var], Stmt)

-- | Constants
data Const
  = NumConst  Double -- All numbers are doubles in ECMAScript
  | CharConst Char
  | StrConst  String
  | BoolConst Bool
  | EmptyListConst

strConst :: String -> Const
strConst = StrConst

-- | Representation of a variable; basically just an ID.
data Var = Var Int | Global String
  deriving Eq

instance Show Var where
  show (Var n)    = '_' : show n
  show (Global s) = s

instance Num Var where
  (Var a) + (Var b) = Var (a+b)
  (Var a) - (Var b) = Var (a-b)
  (Var a) * (Var b) = Var (a*b)
  abs (Var n)       = Var (abs n)
  signum (Var n)    = Var (signum n)
  fromInteger n     = Var $ fromInteger n


-- | A constructor ID
newtype ConstrID = ConstrID Int deriving (Eq, Num, Enum)

instance Show ConstrID where
  show (ConstrID n) = show n
