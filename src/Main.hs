module Main where
import System.Environment (getArgs)
import LambdaScript.Make (make)

main :: IO ()
main = do
  (fp:_) <- getArgs
  make fp "main"
