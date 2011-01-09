module Main where
import System.Environment (getArgs)
import LambdaScript.Make (make)
import Args
import LambdaScript.Config

handlers :: [Handler (Either () Cfg)]
handlers = [
    startsWith "-m"   ==> setModName,
    startsWith "-o"   ==> setOutput,
    startsWith "-L"   ==> setLibDir,
    startsWith "-l"   ==> setExtraLibDirs,
    ((/= '-') . head) ==> setFileName,
    (== "--help")     ==> showHelp
  ]

showHelp _ _ _ =
  Ok $ Left ()
setFileName name _ (Right cfg) =
  Ok $ Right $ cfg {input = name}
setModName =
  withParams 2 $ \(mod:_) (Right a) ->
                   Ok $ Right $ a {forceModName = mod}
setOutput =
  withParams 2 $ \(out:_) (Right a) ->
                   Ok $ Right $ a {output = out}
setLibDir =
  withParams 2 $ \(dir:_) (Right a) ->
                   Ok $ Right $ a {libDir = dir}
setExtraLibDirs =
  withParams 2 $ \dirs (Right a) ->
                   Continue $ Right $ a {extraLibDirs = extraLibDirs a ++ dirs}

main :: IO ()
main = do
  args <- getArgs
  case match (Right defCfg) args handlers of
    Right cfg -> make cfg
    Left _    -> do
      putStrLn $
        "Usage: lsc [options] file\n\n" ++
        "List of options\n" ++
        "-m<name>  Force the name of the module to <name>\n" ++
        "-o<out>   Write the compiled bundle to <out>.js\n" ++
        "-L<dir>   Set the main library directory to <dir>. " ++
                   "<dir> must contain a compatible runtime.js\n" ++
        "-l<dir1,dir2,...>\n" ++
        "          Add the directories dir1, dir2, ... to the library " ++
                   " path."
