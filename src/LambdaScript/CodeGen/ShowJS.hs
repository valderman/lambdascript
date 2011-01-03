-- | Turns LS intermediate representation into JS.
module LambdaScript.CodeGen.ShowJS where
import LambdaScript.CodeGen.Ops
import qualified LambdaScript.Abs as A
import Data.List (intercalate)

instance Show Exp where
  show (Thunk ex)     = "function _(){if(_u(_)) _.x=" ++ show ex ++ ";return _.x;}"
  show (Eval ex)      = show ex ++ "()"
  show (Tailcall ex)  = "_tc(" ++ show ex ++ ")"
  show (Index ex ix)  = show ex ++ "[" ++ show ix ++ "]"
  show (Array exs)    = "[" ++ intercalate "," (map show exs) ++ "]"
  show (ConstrIs e c) = show e ++ "[0]" ++ " == " ++ show c
  show (Cons x xs)    = "[1," ++ show x ++ "," ++ show xs ++ "]"
  show (Const c)      = show c
  show (Ident v)      = show v
  show (Oper o e1 e2) = "(" ++ show e1 ++ ")" ++ show o ++ "(" ++ show e2 ++ ")"
  show (Neg ex)       = "!(" ++ show ex ++ ")"
  show (FunExp f)     = show f
  show (Call f args)  = show f ++ "(" ++
                          intercalate "," (map show args) ++
                          ")"

instance Show Stmt where
  show (Assign v ex) = "var " ++ show v ++ " = " ++ show ex ++ ";\n"
  show (If ex th el) = "if(" ++ show ex ++ ") " ++
                          show th ++ "\n" ++
                          case el of
                            Just ex -> "else " ++ show ex ++ "\n"
                            _       -> ""
  show (Return ex)   = "return " ++ show ex ++ ";\n"
  show (Block stmts) = "{\n" ++ concat (map show stmts) ++ "}"
  show (Forever st)  = "for(;;) " ++ show st
  show (Break)       = "break;\n"

instance Show Oper where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Mod = "%"
  show And = "&&"
  show Or  = "||"
  show Eq  = "=="
  show Lt  = "<"
  show Gt  = ">"
  show Le  = "<="
  show Ge  = ">="
  show Ne  = "!="

instance Show Fun where
  show (FunIdent str)   = str
  show (Lambda as st)   = "function(" ++ intercalate "," (map show as) ++
                          ")" ++ show st ++ "\n"
  show (Construct t id) = "_C(" ++ show (numArgs t) ++ "," ++ show id ++ ")"
    where
      numArgs (A.TOp _ t) = numArgs t + 1
      numArgs _           = 0

instance Show Const where
  show (NumConst d)      = show d
  show (CharConst c)     = ['\'', c, '\'']
  show (StrConst s)      = show s
  show (BoolConst True)  = "1"
  show (BoolConst False) = "0"
  show (EmptyListConst)  = "[0]"
