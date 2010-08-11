module Main where
import System.Environment (getArgs)
import LambdaScript.TypeChecker
import LambdaScript.Par
import LambdaScript.Lex
import LambdaScript.Print
import LambdaScript.ErrM
import LambdaScript.Abs
import LambdaScript.TCM

main :: IO ()
main = do
  (fp:_) <- getArgs
  putStrLn $ "Checking " ++ fp
  parseAndCheck fp

-- | Parse and type check a program.
parseAndCheck :: FilePath -> IO ()
parseAndCheck fp = do
  str <- readFile fp
  let prog = case pProgram $ tokens str of
               Ok p   -> annotate p `runIn'` blankEnv
               Bad s  -> error $ "Err: " ++ s
  case prog of
    Ok (Program p, st) -> putStrLn $ printTree p
    Bad x      -> error $ "Err: " ++ x

-- TODO:
--  * make sure foo :: a conflicts with more specific foo definition
--  * unify pattern types with expression types (check expressions first,
--    obviously, algorithm in TODO2 on erstin)
--  * handle guards (annotateGuardedPattern in annotateDef)
--  * make sure that function defs have the same # of args!
--  * perform alpha conversion on parametrized types before running any
--    checks on them! (addTypes seems like the appropriate place for this)
--  * check that all used type vars are actually declared! (also addTypes?)
--  * after everything else, turn all TUnknown into unbound type vars.
