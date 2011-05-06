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
-- import LambdaScript.Opt.Uncurry as U
-- import LambdaScript.Opt.FoldCalls
-- import LambdaScript.Opt.ClosuresFromFoldedCalls
import LambdaScript.Opt.UnThunkFunc
import LambdaScript.Opt.InlineReturn
import LambdaScript.Opt.InlineJSFun
import LambdaScript.Opt.RemoveDeadCode
import LambdaScript.Opt.TCE

-- | The list of optimizations to apply to the list of functions. Optimizations
--   are applied from left to right.
opts :: [Opt]
opts = [
    noZeroCompares,
    booleanSimplify,
    noObviousIfs,
    zapArrays,
    reduceBlocks,
    noUselessAssigns,
{-  Disable these three until the issue with generics returning functions
    is resolved.
    U.uncurry,
    foldCalls,
    closuresFromFolded,-}
    unEvalGlobals,
    inlineReturn,
    inlineJSFun,
    removeDeadCode
  ]

-- | Apply optimizations to the functions.
--   Something worth noting about tail call elimination, is that
--   foo = bar 10; is NOT a tail call but rather a constant assignment.
applyOpts :: [Function] -> [Function]
applyOpts fs =
  map (eliminateTailCalls . unThunkFunc)
    $ foldl' (\fs o -> optimize o fs) fs opts
