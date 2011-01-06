-- | Whenever we encounter a block containing another block not coupled with
--   another statement (such as an if or a loop) we can safely just merge the
--   blocks since they're guaranteed to not have any overlapping var names.
module LambdaScript.Opt.ReduceBlocks (reduceBlocks) where
import LambdaScript.Opt.Core
import LambdaScript.CodeGen.Ops

reduceBlocks :: Opt
reduceBlocks = Opt {
    optStm = reduce,
    optExp = id
  }

isBlock :: Stmt -> Bool
isBlock (Block _) = True
isBlock _         = False

reduce :: Stmt -> Stmt
reduce ex@(Block ss) =
  case span (not . isBlock) ss of
    (pre, Block b : post) -> reduce $ Block $ pre ++ b ++ post
    _                     -> ex
reduce x                  = x