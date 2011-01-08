-- | Turn all \a -> \b -> ... functions into \a b -> ...
--   Note that this optimization depends on the FoldCalls and
--   ClosuresFromFoldedCalls optimizations to generate correct code.
module LambdaScript.Opt.Uncurry (LambdaScript.Opt.Uncurry.uncurry) where
import LambdaScript.Opt.Core
import LambdaScript.CodeGen.Ops

uncurry :: Opt
uncurry = Opt {
    optStm = id,
    optExp = uncurry'
  }

delLast :: [a] -> [a]
delLast (x:xs@(_:_)) = x:delLast xs
delLast _          = []

-- | Inlines any functions at the end of this function.
--   TODO:
--   Since constructors are handled using a runtime helper function that's
--   able to take its arguments either like C(1,2,3) or C(1)(2)(3) we don't
--   need to consider the constructor case for correctness, but we definitely
--   should for performance.
uncurry' :: Exp -> Exp
uncurry' ex@(FunExp (Lambda vs (Block ss))) =
  case last ss of
    Return n (FunExp (Lambda vs' (Block ss'))) ->
      FunExp (Lambda (vs ++ vs') (Block (delLast ss ++ ss')))
    Return n (FunExp (FunIdent id)) ->
      inlineFunIdent n (Ident $ Global 1 id)
    Return n v@(Ident _) ->
      inlineFunIdent n v
    -- Only idents are ever thunks, so it can't be anything but an ident.
    Return n v@(Eval (Ident _)) ->
      inlineFunIdent n v
    _ ->
      ex
  where
    -- | Inline a function identifier. If the function has arity 0 we just return
    --   it. Otherwise, we choose n new vars (since we use a different naming
    --   convention there will be no name clashes, and since we only just
    --   introduced them they aren't used in any inner lambdas and so there will
    --   be no problems with name capture) to add to the parent function's argument
    --   list, and return the result of calling the returned function with those
    --   arguments. Since the arguments come straight from the parent's argument
    --   list, they're already thunked so we don't need to thunk them again.
    inlineFunIdent :: Int -> Exp -> Exp
    inlineFunIdent n var =
      let vs' = take n newVars
          ss' = if n == 0
                  then [Return n var]
                  else [Return 0 $ Call n var (map Ident vs')]
      in  FunExp $ Lambda (vs ++ vs') (Block (delLast ss ++ ss'))
uncurry' x = x
