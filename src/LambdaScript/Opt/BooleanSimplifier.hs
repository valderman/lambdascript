-- | Simplify boolean expressions.
module LambdaScript.Opt.BooleanSimplifier (booleanSimplify) where
import LambdaScript.CodeGen.Ops
import LambdaScript.CodeGen.ShowJS -- for error messages
import LambdaScript.Opt.Core

isTrue :: Exp -> Bool
isTrue (Const (BoolConst True)) = True
isTrue _                        = False

isFalse :: Exp -> Bool
isFalse (Const (BoolConst False)) = True
isFalse _                         = False

true :: Exp
true = Const (BoolConst True)

false :: Exp
false = Const (BoolConst False)

booleanSimplify :: Opt
booleanSimplify = Opt {
    optStm = id,
    optExp = simplify
  }

-- | Recursively simplify an expression.
simplify :: Exp -> Exp
simplify (Oper And a b)
  | isTrue a && isTrue b = true
  | isTrue a             = b
  | isTrue b             = a
  | otherwise            = Oper And a b
simplify (Oper Or a b)
  | isFalse a && isFalse b = false
  | isFalse a              = b
  | isFalse b              = a
  | otherwise              = Oper Or a b
simplify (Oper Eq a b)
  | isTrue a && isTrue b   = true
  | isFalse a && isFalse b = true
  | isTrue a               = b
  | isTrue b               = a
  | isFalse a              = Neg b
  | isFalse b              = Neg a  
simplify (Neg a)
  | isTrue a  = false
  | isFalse a = true
  | otherwise = Neg a
simplify x =
  x