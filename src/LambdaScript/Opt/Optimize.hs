-- | Apply optimizations.
module LambdaScript.Opt.Optimize (applyOpts) where
import Data.List (foldl')
import LambdaScript.CodeGen.Module
import LambdaScript.Opt.Core
import LambdaScript.Opt.BooleanSimplifier

-- | The list of optimizations to apply to the list of functions.
opts :: [Opt]
opts = [
    booleanSimplify
  ]

-- | Apply optimizations to the functions.
applyOpts :: [Function] -> [Function]
applyOpts fs =
  foldl' (\fs o -> optimize o fs) fs opts
