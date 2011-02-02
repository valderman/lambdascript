-- | Turns LS intermediate representation into JS.
module LambdaScript.CodeGen.ShowJS where
import LambdaScript.CodeGen.Ops
import qualified LambdaScript.Abs as A
import Data.List (intercalate)

-- | Collect statements from every StmtEx that should be executed here.
--   Basically, collect everything until we reach either an if, a thunk or a
--   lambda. We also don't go into the args of a function call, since those
--   are all guaranteed to end in thunks anyway.
getSS :: Exp -> [Stmt]
getSS (StmtEx ss ex)    = ss ++ getSS ex
getSS (Eval ex)         = getSS ex
getSS (Tailcall ex)     = getSS ex
getSS (Index ex ix)     = getSS ex ++ getSS ix
getSS (Array exs)       = concat $ map getSS exs
getSS (ConstrIs ex _ _) = getSS ex
getSS (Cons x xs)       = getSS x ++ getSS xs
getSS (Oper _ e1 e2)    = getSS e1 ++ getSS e2
getSS (Neg ex)          = getSS ex
getSS (Call _ f _)      = getSS f
getSS _                 = []

showSS :: Exp -> String
showSS = concat . map show . getSS

instance Show Exp where
  show (Thunk ex) =
    "function _(){if(_.x===$u){" ++ showSS ex ++ "_.x=" ++ show ex ++ ";}return _.x;}"
  show (IOThunk ex) =
    "function _(){" ++ showSS ex ++ "return " ++ show ex ++ ";}"
  show (StmtEx ss ex) =
    show ex
  show (Eval ex) =
    show ex ++ "()"
  show (Tailcall ex) =
    "_tc(" ++ show ex ++ ")"
  show (Index ex ix) =
    show ex ++ "[" ++ show ix ++ "]"
  show (Array exs) =
    "[" ++ intercalate "," (map show exs) ++ "]"
  show (ConstrIs e 0 v) =
    "!(" ++ show v ++ "=" ++ show e ++ ")[0]"
  show (ConstrIs e c v) =
    "(" ++ show v ++ "=" ++ show e ++ ")[0]" ++ " == " ++ show c
  show (Cons x xs) =
    "[1," ++ show x ++ "," ++ show xs ++ "]"
  show (Const c) =
    show c
  show (Ident v) =
    show v
  show (Oper o e1 e2) =
    "(" ++ show e1 ++ ")" ++ show o ++ "(" ++ show e2 ++ ")"
  show (Neg ex) =
    "!(" ++ show ex ++ ")"
  show (FunExp f) =
    show f
  show (Call _ f args) =
    show f ++ "(" ++
        intercalate "," (map show args) ++
      ")"
  show (NoExp) =
    ""
  show x =
    error $ "No Show instance for some Exp!"

instance Show Stmt where
  show (SelfThunk id ss) =
    "if($u===$._" ++ id ++ ".x){" ++ concat (map show ss) ++ ";}return $._" ++ id ++ ".x;"
  show (Assign v@(Global _ id) ex) = showSS ex ++ show v ++ " = " ++ show ex ++ ";\n"
  show (Assign v ex)  = showSS ex ++ "var " ++ show v ++ " = " ++ show ex ++ ";\n"
  show (If ex th el)  = showSS ex++ 
                        "if(" ++ show ex ++ ") " ++
                           show th ++ "\n" ++
                           case el of
                             Just ex -> "else " ++ show ex ++ "\n"
                             _       -> ""
  show (Return _ ex)  = showSS ex ++ "return " ++ show ex ++ ";\n"
  show (Block stmts)  = "{\n" ++ concat (map show stmts) ++ "}"
  show (NoStmt)       = ""
  show (ExpStmt ex)   = showSS ex ++ show ex ++ ";\n"
  show (Forever body) = "for(;;){\n" ++ show body ++ "}\n"
  show (Break)        = "break;\n"
  show x              = error $ "No Show instance for some Stmt!"

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
  show x   = error $ "No Show instance for some operator!"

instance Show Fun where
  show (FunIdent str)   = str
  show (Lambda as st)   = "function(" ++ intercalate "," (map show as) ++
                          ")" ++ show st ++ "\n"
  show (Construct t id) = "_C(" ++ show (numArgs t) ++ "," ++ show id ++ ")"
    where
      numArgs (A.TOp _ t) = numArgs t + 1
      numArgs _           = 0
  show x                = error $ "No Show instance for some Fun!"

instance Show Const where
  show (NumConst d)       = if snd (properFraction d) /= 0
                               then show d
                               else show (truncate d)
  show (CharConst c)      = ['\'', c, '\'']
  show (StrConst s)       = "_s(" ++ show s ++ ")"
  show (InlineStrConst s) = s
  show (BoolConst True)   = "1"
  show (BoolConst False)  = "0"
  show (EmptyListConst)   = "[0]"
  show x                  = error $ "No Show instance for some Const!"
