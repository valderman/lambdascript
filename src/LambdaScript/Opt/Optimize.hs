-- | Apply optimizations.
module LambdaScript.Opt.Optimize (applyOpts) where
import Data.List (foldl')
import LambdaScript.CodeGen.Module
import LambdaScript.Opt.Core
import LambdaScript.Opt.BooleanSimplifier
import LambdaScript.Opt.NoZeroCompares
import LambdaScript.Opt.NoObviousIfs
import LambdaScript.Opt.ReduceBlocks
import LambdaScript.Opt.NoUselessAssigns

-- | The list of optimizations to apply to the list of functions. Optimizations
--   are applied from left to right.
opts :: [Opt]
opts = [
    noZeroCompares,
    booleanSimplify,
    noObviousIfs,
    reduceBlocks,
    noUselessAssigns
  ]

-- | Apply optimizations to the functions.
applyOpts :: [Function] -> [Function]
applyOpts fs =
  foldl' (\fs o -> optimize o fs) fs opts
