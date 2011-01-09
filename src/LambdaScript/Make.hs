-- | Constructs a list of modules to build, orders it by dependency, then
--   builds all of them in turn.
module LambdaScript.Make (make) where
import qualified Data.Map as Map
import Data.List (nub, foldl')
import LambdaScript.Desugar (desugar)
import LambdaScript.Depends (bindGroups)
import LambdaScript.Par (pModule)
import LambdaScript.Lex (tokens)
import LambdaScript.Abs as Abs
import qualified LambdaScript.CodeGen.Module as M
import LambdaScript.ErrM
import LambdaScript.Types (Assump (..), Assumps, find)
import LambdaScript.TypeChecker (infer)
import LambdaScript.CodeGen.Generate (generate)
import LambdaScript.Opt.Optimize (applyOpts)
import LambdaScript.Config (Cfg (..))
import System.Directory (doesFileExist)

-- TODO:
-- * Detect and error out on dependency cycles.
-- * Make sure modules only get the imports they explicitly ask for. At the
--   moment, a module gets not only the asked-for imports, but also the imports
--   from each of its imports' imports and so on.

-- | Build a lambdascript file and all its dependencies.
make :: Cfg -> IO ()
make cfg = do
  let fp = input cfg
      forcedName = forceModName cfg
      inputdir = case reverse $ drop 1 $ dropWhile (/= '/') $ reverse fp of
                   "" -> "."
                   s  -> s
      libpath = inputdir : extraLibDirs cfg ++ [libDir cfg]
  (names, mods) <- buildModList fp libpath forcedName >>= return . unzip
  -- Create a module => exported functions mapping.
  -- If a module lacks an export statement, export nothing to othermodules.
  -- However, EVERYTHING is later exported to the user.
  let getExs (Module (Exports es) _ _) = es
      getExs _                           = []
      exports = Map.fromList $ zip names (map getExs mods)
      -- Create a list of imports, in the same order as names and mods, with
      -- the functions each module imports.
      imports = map (mkImpList exports) mods
      mods' = zipWith3 genModule names imports (checkList mods)
  writeBundle (output cfg ++ ".js") (libDir cfg) mods'
  where
    -- Create the list of imports for the given module.
    mkImpList es (Module _ is _) =
      [(mod, fun) | Import (VIdent mod) <- is,
                    Export (VIdent fun) <- es Map.! mod]

-- | Write a compiled module to file.
writeBundle :: FilePath -> FilePath -> [M.Module] -> IO ()
writeBundle fp libpath mods = do
  runtime <- readFile $ libpath ++ "/runtime.js"
  -- Strip comments from the runtime
  let rt = unlines [x | x <- lines runtime, not (null x) && take 2 x /= "//"]
  writeFile fp $ rt ++ concat (map show mods)

-- | Generate code for a module.
genModule :: String             -- ^ Filename of the module to generate code
                                --   for, sans extension.
          -> [(String, String)] -- ^ (module, function) list of imports.
          -> Abs.Module         -- ^ The module itself in AST form.
          -> M.Module           -- ^ The final module.
genModule n is (Module exs _ p) =
  M.Module {
      M.modName = n,
      M.funcs   = funcs,
      M.exports = exs'
    }
  where
    funcs = applyOpts $ generate is p

    -- If there is no export list, export everything to the user.
    -- However, nothing is visible to other modules.
    exs' = case exs of
      Exports exs -> map (\(Export (VIdent id)) -> id) exs
      _           -> map M.funName funcs

-- | Returns the path of the file containing the given module.
file :: String      -- ^ Module to find file for.
     -> [FilePath]  -- ^ List of library search paths.
     -> IO FilePath
file mod (p:paths) = do
  let path = p ++ "/" ++ mod ++ ".ls"
  exists <- doesFileExist path
  if exists
    then return path
    else file mod paths
file m _ =
  error $ "The module " ++ m ++
          " could not be found on any given library path!"

-- | Builds a list of all modules that should be compiled.
--   The list is dependency ordered.
buildModList :: FilePath   -- ^ Base module to compile
             -> [FilePath] -- ^ List of directories to search for modules.
             -> String     -- ^ If a name different from the file name is to be
                           --   used for the base module, this is it.
             -> IO [(String, Abs.Module)]
buildModList fp libpaths forceName = do
  str <- readFile fp
  -- Take the base module, parse it enough to extract its dependencies, do the
  -- same to the dependencies recursively to obtain a dependency list, then
  -- append the base module to the end of the list.
  (deps, mod) <- case pModule $ tokens str of
                   Ok m@(Module exs imports _) -> do
                     d <- mapM (\(Import (VIdent i)) ->
                                 file i libpaths >>= \f -> buildModList f libpaths "")
                               imports
                     let name = if (not . null) forceName
                                  then forceName
                                  else modName fp
                     return (d, (name, m))
                   Bad s                ->
                     fail $ "Compilation of " ++ fp ++ " failed: " ++ s
  -- nub removes duplicates at the end, not the beginning, which ensures that
  -- the list stays sorted in dependency order.
  -- It's quite possible to go into a dependency cycle here; do something
  -- about that later.
  return $ nub $ concat deps ++ [mod]

-- | Prepare a module for type checking by performing desugaring and dependency
--   grouping of bindings.
prepare :: Abs.Module -> Abs.Module
prepare (Module exs is p) =
  Module exs is (bindGroups $ desugar p)

-- | Type check and annotate a module. Apart from the annotated module, also
--   return the list of assumptions for the module.
typeCheck :: Assumps -> Abs.Module -> (Abs.Module, Assumps)
typeCheck as (Module exs is p) =
  case infer as p of
    (p', as') -> (Module exs is p', prune as')
  where
    -- Only keep symbols that we either got from another dependency or that
    -- we exported ourselves in the list of assumptions.
    prune as' =
      allSyms (map (\(id :>: _) -> id) as ++
               map (\(Export (VIdent x)) -> x) exs')
              as'
    exs' = case exs of
      Exports exs -> exs
      _           -> []
    allSyms xs ys =
      concat $ map (\x -> filter (\(id :>: _) -> x == id) ys) xs

-- | Type check a dependency ordered list of modules.
checkList :: [Abs.Module] -> [Abs.Module]
checkList ms =
  reverse $ snd $ foldl' check ([],[]) ms
  where
    check (as, ms) m =
      case typeCheck as $ prepare m of
        (m', as') -> (as', m':ms)

-- | Returns the module name of the given file.
modName :: FilePath -> String
modName = reverse
  . takeWhile (/= '/')
  . tail
  . dropWhile (/= '.')
  . reverse
