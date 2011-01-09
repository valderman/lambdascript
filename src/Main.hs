module Main where
import System.Environment (getArgs)
import LambdaScript.Desugar (desugar)
import LambdaScript.Depends (bindGroups)
import LambdaScript.Par (pModule, pProgram)
import LambdaScript.Lex (tokens)
import LambdaScript.Abs as Abs (Module (..), Program)
import LambdaScript.ErrM
import LambdaScript.TypeChecker (infer)
import LambdaScript.CodeGen.Generate (generate)
import LambdaScript.Opt.Optimize (applyOpts)
import LambdaScript.CodeGen.Module as M

main :: IO ()
main = do
  (fp:_) <- getArgs
  p <- parseAndCheck fp
  -- Really ugly way of ensuring p' is evaluated enough to spot any errors
  -- before we start writing any code to file without having to write NFData
  -- instances for the intermediate code types.
  case generate [] p of
    p' | length p' /= -1 -> do
      let mod = reverse
              . tail
              . takeWhile (/= '/')
              . dropWhile (/= '.')
              $ (reverse fp)
          jsfile = mod ++ ".js"
      runtime <- readFile "lib/runtime.js"
      let rt = unlines [x | x <- lines runtime, not (null x) && take 2 x /= "//"]
          p'' = applyOpts p'
          allFuncs = map M.funName p''
      writeFile jsfile $ rt ++ show (M.Module "main" allFuncs p'')

-- | Parse and type check a program.
parseAndCheck :: FilePath -> IO Program
parseAndCheck fp = do
  str <- readFile fp
  let (prog, subst) =
        case pProgram $ tokens str of
          Ok p  -> infer [] $ bindGroups $ desugar p
          Bad s -> error $ "Err: " ++ s
  return prog
