module Main where
import System.Environment (getArgs)
import qualified Data.Map as M
import LambdaScript.Desugar (desugar)
import LambdaScript.Depends (bindGroups)
import LambdaScript.Par (pProgram)
import LambdaScript.Lex (tokens)
import LambdaScript.Abs (Program)
import LambdaScript.ErrM
import LambdaScript.TypeChecker (infer)
import LambdaScript.CodeGen.Generate (generate)

main :: IO ()
main = do
  (fp:_) <- getArgs
  p <- parseAndCheck fp
  -- Really ugly way of ensuring p' is evaluated enough to spot any errors
  -- before we start writing any code to file without having to write NFData
  -- instances for the intermediate code types.
  case generate p of
    p' | length p' /= -1 -> do
      let jsfile = reverse
                 . ("sj" ++) 
                 . takeWhile (/= '/')
                 . dropWhile (/= '.')
                 $ (reverse fp)
      runtime <- readFile "lib/runtime.js"
      let rt = unlines [x | x <- lines runtime, not (null x) && take 2 x /= "//"]
      writeFile jsfile $ rt ++ show p'

-- | Parse and type check a program.
parseAndCheck :: FilePath -> IO Program
parseAndCheck fp = do
  str <- readFile fp
  let (prog, subst) =
        case pProgram $ tokens str of
          Ok p   -> infer $ bindGroups $ desugar p
          Bad s  -> error $ "Err: " ++ s
  return prog
