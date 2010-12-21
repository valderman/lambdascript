module Main where
import System.Environment (getArgs)
import qualified Data.Map as M
import LambdaScript.Desugar
import LambdaScript.Depends
import LambdaScript.Par
import LambdaScript.Lex
import LambdaScript.Abs
import LambdaScript.Print
import LambdaScript.ErrM
import LambdaScript.TypeChecker
import LambdaScript.CodeGen.Generate

main :: IO ()
main = do
  (fp:_) <- getArgs
  putStrLn $ "Checking " ++ fp
  p <- parseAndCheck fp
  putStrLn $ printTree p
  putStrLn "\n\n\n\n"
  putStrLn $ show $ generate p

-- | Parse and type check a program.
parseAndCheck :: FilePath -> IO Program
parseAndCheck fp = do
  str <- readFile fp
  let (prog, subst) =
        case pProgram $ tokens str of
          Ok p   -> infer $ bindGroups $ desugar p
          Bad s  -> error $ "Err: " ++ s
  return prog