-- | Apply optimizations.
module LambdaScript.Opt.Optimize (applyOpts) where
import Data.List (foldl')
import LambdaScript.CodeGen.Module
import LambdaScript.Opt.Core
import LambdaScript.Opt.BooleanSimplifier
import LambdaScript.Opt.NoZeroCompares
import LambdaScript.Opt.NoObviousIfs
import LambdaScript.Opt.ZapArrays
import LambdaScript.Opt.ReduceBlocks
import LambdaScript.Opt.NoUselessAssigns
import LambdaScript.Opt.Uncurry as U

-- | The list of optimizations to apply to the list of functions. Optimizations
--   are applied from left to right.
opts :: [Opt]
opts = [
    noZeroCompares,
    booleanSimplify,
    noObviousIfs,
    zapArrays,
    reduceBlocks,
    noUselessAssigns
    -- U.uncurry -- we can't turn this on until we make all function calls aware.
  ]

-- | Apply optimizations to the functions.
applyOpts :: [Function] -> [Function]
applyOpts fs =
  foldl' (\fs o -> optimize o fs) fs opts
