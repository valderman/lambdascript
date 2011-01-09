module Main where
import System.Environment (getArgs)
import LambdaScript.Make (make)
import Args
import LambdaScript.Config

handlers = [
    startsWith "-m"   ==> setModName,
    startsWith "-o"   ==> setOutput,
    startsWith "-L"   ==> setLibDir,
    startsWith "-l"   ==> setExtraLibDirs,
    ((/= '-') . head) ==> setFileName
  ]

setFileName name _ cfg =
  Ok $ cfg {input = name}
setModName =
  withParams 2 $ \(mod:_) a -> Ok $ a {forceModName = mod}
setOutput =
  withParams 2 $ \(out:_) a -> Ok $ a {output = out}
setLibDir =
  withParams 2 $ \(dir:_) a -> Ok $ a {libDir = dir}
setExtraLibDirs =
  withParams 2 $ \dirs a ->
                    Continue $ a {extraLibDirs = extraLibDirs a ++ dirs}

main :: IO ()
main = do
  args <- getArgs
  make $ match defCfg args handlers
