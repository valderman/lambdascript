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
    -- The first argument of the call primitive is the maximum number of args
    -- that may be passed to the function; the second argument is the function
    -- to be called, and the third is the list of arguments.
    -- The max # of args bit is needed to optimize \a -> \b -> ...
    -- into \a b -> ...
  | Call      Int Exp [Exp]
  | NoExp

data Stmt
  = Assign Var Exp
  | SelfThunk String [Stmt]
  | If      Exp Stmt (Maybe Stmt)
    -- The first argument of the return primitive is the arity of the returned
    -- expression. The optimizer needs this information to inline stuff like
    -- mapFOver = map f
  | Return  Int Exp
  | Block   [Stmt]
  | Forever Stmt
  | Break
  | NoStmt

-- | Operators
data Oper
  = Add | Sub | Mul | Div | Mod
  | And | Or
  | Eq  | Lt  | Gt  | Le  | Ge | Ne

-- | Functions
data Fun
  = FunIdent  String
  | Lambda    [Var]  Stmt
  | Construct Type   ConstrID

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
--   A global consists of an arity and a name, and a temp var is just a name.
data Var = Var Int | Global Int String | Temp String
  deriving Eq

instance Show Var where
  show (Var n)      = '_' : show n
  show (Global _ s) = s
  show (Temp s)     = s

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
