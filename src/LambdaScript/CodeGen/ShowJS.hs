{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
-- | Turns LS intermediate representation into JS.
module LambdaScript.CodeGen.ShowJS where
import LambdaScript.Config (Cfg (..))
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
getSS (Index ex ix)     = getSS ex ++ getSS ix
getSS (Array exs)       = concat $ map getSS exs
getSS (ConstrIs ex _ _) = getSS ex
getSS (Cons x xs)       = getSS x ++ getSS xs
getSS (Oper _ e1 e2)    = getSS e1 ++ getSS e2
getSS (Neg ex)          = getSS ex
getSS (Call _ f _)      = getSS f
getSS _                 = []

showSS :: Cfg -> Exp -> String
showSS c = concat . map (showJS c) . getSS

class ShowJS a where
  showJS :: Cfg -> a -> String

instance ShowJS String where
  showJS _ = show

instance ShowJS Double where
  showJS _ = show

instance ShowJS Int where
  showJS _ = show

instance ShowJS [Exp] where
  showJS c exs =
    "[" ++ intercalate "," (map (showJS c) exs) ++ "]"

instance ShowJS ConstrID where
  showJS _ = show

instance ShowJS Var where
  showJS _ = show

instance ShowJS Exp where
  showJS c (Thunk ex) =
    "{x:function(){" ++ showSS c ex ++ "this.e=1;this.x=" ++ showJS c ex ++ ";return this.x;}}"
  showJS c (IOThunk ex) =
    "{x:function(){" ++ showSS c ex ++ "return " ++ showJS c ex ++ ";}}"
  showJS c (StmtEx ss ex) =
    showJS c ex
  showJS c (Eval ex) =
    let x = showJS c ex
     in  "(" ++ x ++ ".e?" ++ x ++ ".x:" ++ x ++ ".x())"
  showJS c (Index ex ix) =
    showJS c ex ++ "[" ++ showJS c ix ++ "]"
  showJS c (Array exs) =
    "[" ++ intercalate "," (map (showJS c) exs) ++ "]"
  showJS c (ConstrIs e 0 v) =
    "!(" ++ showJS c v ++ "=" ++ showJS c e ++ ")[0]"
  showJS cfg (ConstrIs e c v) =
    "(" ++ showJS cfg v ++ "=" ++ showJS cfg e ++ ")[0]" ++ " == " ++ showJS cfg c
  showJS c (Cons x xs) =
    "[1," ++ showJS c x ++ "," ++ showJS c xs ++ "]"
  showJS cfg (Const c) =
    showJS cfg c
  showJS c (Ident v) =
    showJS c v
  showJS c (Oper o e1 e2) =
    "(" ++ showJS c e1 ++ ")" ++ showJS c o ++ "(" ++ showJS c e2 ++ ")"
  showJS c (Neg ex) =
    "!(" ++ showJS c ex ++ ")"
  showJS c (FunExp f) =
    showJS c f
  showJS c (Call _ f args) 
    | tailcalls c =
      "_t(" ++ showJS c f ++ "," ++ showJS c args ++ ")"
    | otherwise =
      showJS c f ++ "(" ++ intercalate "," (map (showJS c) args) ++ ")"
  showJS c (NoExp) =
    ""
  showJS c x =
    error $ "No ShowJS instance for some Exp!"

instance ShowJS Stmt where
  showJS c (Tailcall ex args)
    | tailcalls c =
      "return {f:" ++ showJS c ex ++ ",a:" ++ showJS c args ++ "};"
    | otherwise =
      "return " ++ showJS c ex ++ "(" ++ intercalate "," (map (showJS c) args) ++ ");"
  showJS c (SelfThunk id ss) =
    concat (map (showJS c) ss) ++ "return $._" ++ id ++ ".x;"
  showJS c (Assign v@(Global _ id) ex) = showSS c ex ++ showJS c v ++ " = " ++ showJS c ex ++ ";\n"
  showJS c (Assign v ex)  = showSS c ex ++ "var " ++ showJS c v ++ " = " ++ showJS c ex ++ ";\n"
  showJS c (AssignResult v ex) =
    showSS c ex ++ "var " ++ showJS c v ++ " = " ++ showJS c ex ++ ";\n"
  showJS c (If ex th el)  = showSS c ex++ 
                        "if(" ++ showJS c ex ++ ") " ++
                           showJS c th ++ "\n" ++
                           case el of
                             Just ex -> "else " ++ showJS c ex ++ "\n"
                             _       -> ""
  showJS c (Return _ ex)  = showSS c ex ++ "return " ++ showJS c ex ++ ";\n"
  showJS c (Block stmts)  = "{\n" ++ concat (map (showJS c) stmts) ++ "}"
  showJS c (NoStmt)       = ""
  showJS c (ExpStmt ex)   = showSS c ex ++ showJS c ex ++ ";\n"
  showJS c (Forever body) = "for(;;){\n" ++ showJS c body ++ "}\n"
  showJS c (Break)        = "break;\n"
  showJS c x              = error $ "No ShowJS instance for some Stmt!"

instance ShowJS Oper where
  showJS c Add = "+"
  showJS c Sub = "-"
  showJS c Mul = "*"
  showJS c Div = "/"
  showJS c Mod = "%"
  showJS c And = "&&"
  showJS c Or  = "||"
  showJS c Eq  = "=="
  showJS c Lt  = "<"
  showJS c Gt  = ">"
  showJS c Le  = "<="
  showJS c Ge  = ">="
  showJS c Ne  = "!="
  showJS c x   = error $ "No ShowJS instance for some operator!"

instance ShowJS Fun where
  showJS c (FunIdent str)   = str
  showJS c (Lambda as st)   = "function(" ++ intercalate "," (map (showJS c) as) ++
                          ")" ++ showJS c st ++ "\n"
  showJS c (Construct t id) = "_C(" ++ showJS c (numArgs t::Int) ++ "," ++ showJS c id ++ ")"
    where
      numArgs (A.TOp _ t) = numArgs t + 1
      numArgs _           = 0
  showJS c x                = error $ "No ShowJS instance for some Fun!"

instance ShowJS Const where
  showJS c (NumConst d)       = if snd (properFraction d) /= 0
                               then showJS c d
                               else showJS c (truncate d::Int)
  showJS cfg (CharConst c)    = ['\'', c, '\'']
  showJS c (StrConst s)       = "_s(" ++ showJS c s ++ ")"
  showJS c (InlineStrConst s) = s
  showJS c (BoolConst True)   = "1"
  showJS c (BoolConst False)  = "0"
  showJS c (EmptyListConst)   = "[0]"
  showJS c x                  = error $ "No ShowJS instance for some Const!"
