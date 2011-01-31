-- | Replace all calls to _jsfun <string literal> a call to an inlined version
--   that takes its argument as a raw JS string, takes no arity and can NOT be
--   partially applied. (Not that partially applying _jsfun before was
--   particularly safe, but this makes it explicit.)
module LambdaScript.Opt.InlineJSFun (inlineJSFun) where
import LambdaScript.CodeGen.Ops
import LambdaScript.Opt.Core

inlineJSFun :: Opt
inlineJSFun = Opt {
    optStm = id,
    optExp = inline
  }

inline :: Exp -> Exp
inline (Call _ (Call _ (Ident (Builtin "_jsfun")) [Thunk (Const (StrConst fun))]) _) =
  Call 0 (Ident (Builtin "$jsfun")) [(Const $ InlineStrConst fun)]
inline (Call _ (Call _ (Ident (Builtin "_rawjsfun")) [Thunk (Const (StrConst fun))]) _) =
  Call 0 (Ident (Builtin "_rawjsfun")) [(Const $ InlineStrConst fun)]
inline (Call _ (Call _ (Ident (Builtin "$jsfun")) args) next) =
  Call 0 (Ident (Builtin "$jsfun")) (args ++ next)
inline x =
  x