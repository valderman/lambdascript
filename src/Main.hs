module Main where
import System.Environment (getArgs)
import LambdaScript.Desugar
import LambdaScript.Depends
import LambdaScript.Par
import LambdaScript.Lex
import LambdaScript.Print
import LambdaScript.ErrM

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
               Ok p   -> bindGroups $ desugar p
               Bad s  -> error $ "Err: " ++ s
  putStrLn $ printTree prog
