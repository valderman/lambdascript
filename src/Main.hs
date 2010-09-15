module Main where
import System.Environment (getArgs)
import LambdaScript.Desugar
import LambdaScript.Depends
import LambdaScript.Par
import LambdaScript.Lex
import LambdaScript.Print
import LambdaScript.ErrM
import LambdaScript.TypeChecker

main :: IO ()
main = do
  (fp:_) <- getArgs
  putStrLn $ "Checking " ++ fp
  parseAndCheck fp

-- | Parse and type check a program.
parseAndCheck :: FilePath -> IO ()
parseAndCheck fp = do
  str <- readFile fp
  let (prog, subst) =
        case pProgram $ tokens str of
          Ok p   -> infer $ bindGroups $ desugar p
          Bad s  -> error $ "Err: " ++ s
  putStrLn $ unlines $ map show subst
  putStrLn $ printTree $ prog
