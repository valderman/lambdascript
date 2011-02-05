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
import LambdaScript.Types (Assump (..), Assumps, find, arity, ID, Scheme (..), quantifyAll, Instantiate (..))
import LambdaScript.TypeChecker (infer)
import LambdaScript.CodeGen.Generate (generate)
import LambdaScript.Opt.Optimize (applyOpts)
import LambdaScript.Config (Cfg (..))
import System.Directory (doesFileExist)
import Data.Maybe (isJust)

-- TODO:
-- * Detect and error out on dependency cycles.
-- * Warn when a local definition shadows an imported one and, preferrably,
--   add some way of resolving such ambiguities.

-- | Build a lambdascript file and all its dependencies.
make :: Cfg -> IO ()
make cfg = do
  let fp = input cfg
      forcedName = forceModName cfg
      inputdir = case reverse $ drop 1 $ dropWhile (/= '/') $ reverse fp of
                   "" -> "."
                   s  -> s
      libpath = inputdir : extraLibDirs cfg ++ [libDir cfg]
  namesMods <- buildModList fp libpath forcedName
  
  -- Create a module => exported functions mapping.
  -- If a module lacks an export statement, export nothing to othermodules.
  -- However, EVERYTHING is later exported to the user.
  let getExs (Module (Exports es) _ _) = es
      getExs _                         = []
      (names, mods) = unzip namesMods
      exports = Map.fromList $ zip names (map getExs mods)
      -- Create a list of imports, in the same order as names and mods, with
      -- the functions each module imports.
      imports = map (mkImpList exports) mods
      (checkedMods, funmap, typemap) = checkList namesMods
      -- Get the arity of the given function in the given module
      ar m n = case funmap Map.! m Map.! n of
                 Forall _ t -> arity t
      imports' = map (map (\(m, n) -> (ar m n, m, n))) imports
      mods' = zipWith3 genModule names imports' (map snd checkedMods)
  writeBundle (output cfg ++ ".js") (libDir cfg) mods'
  
  where
    -- Create the list of imports for the given module.
    mkImpList es (Module _ is _) =
      [(mod, fun) | Import (VIdent mod) <- is,
                    ExpFun (VIdent fun) <- es Map.! mod]

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
          -> [(Int, String, String)] -- ^ (module, function) list of imports.
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
      Exports exs -> [id | ExpFun (VIdent id) <- exs]
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
typeCheck :: Assumps -> [NewType] -> Abs.Module -> Abs.Module
typeCheck as ts mod@(Module exs _ _) =
  case infer as ts mod of
    (mod', as', ts') -> mod'

-- | Type check a dependency ordered list of modules.
checkList :: [(String, Abs.Module)]
          -> ([(String, Abs.Module)], Map.Map String (Map.Map String Scheme), Map.Map String [NewType])
checkList mods =
  case foldl' check ([], Map.empty, Map.empty) mods of
    (mods, funs, types) -> (reverse mods, funs, types)
  where
    check (mods, expFuns, expTypes) (name, mod) =
      case typeCheck (importedFuns expFuns mod)
                     (importedTypes expTypes mod)
                     (prepare mod) of
        mod' -> ((name, mod'):mods,
                 exportMap expFuns name mod',
                 typeMap expTypes name mod')

    -- Create a list of the functions the given module should import.
    importedFuns expFuns (Module _ imports _) =
      [k :>: v | Import (VIdent imp) <- imports,
                 (k, v) <- Map.toList $ expFuns Map.! imp]

    -- Create a list of the types the given module should import
    importedTypes expTypes (Module _ imports _) =
      concat [expTypes Map.! imp| Import (VIdent imp) <- imports]

    -- Create a list of all the types the given module exports.
    modTypeList (Module (Exports ex) _ (Program defs)) =
      [theType | TypeDef t@(NewType _ _ _) <- defs,
                 theType <- fullOrPartial t ex]
    
    -- Returns the given type, exported, either fully or partially according to
    -- the export list. Or not at all. We use lists instead of Maybe so we can
    -- use it with a list comprehension.
    fullOrPartial t@(NewType (TIdent id) vars args) exs =
      case typeExpMode id exs of
        Just (ExpType _)     -> [NewType (TIdent id) vars []]
        Just (ExpFullType _) -> [t]
        _                    -> []

    -- Return the export mode of the given type, if exported at all.
    typeExpMode id (ex:exs) = 
      case ex of
        ExpType (TIdent id')     | id == id' -> Just ex
        ExpFullType (TIdent id') | id == id' -> Just ex
        _                                    -> typeExpMode id exs
    typeExpMode _ _ =
      Nothing

    -- Create a new module => exported types map, based on the old one and a
    -- new module.
    typeMap oldmap name mod = Map.insert name (modTypeList mod) oldmap

    -- Create a map of all the functions a given module exports.
    modExportMap name (Module (Exports exs) _ (Program bgs)) =
      foldl' (flip $ uncurry Map.insert) Map.empty $
          [(id, quantifyAll t) | BGroup (BindGroup defs) <- bgs,
                                 ConstDef (Ident id) (ETyped _ t) <- defs,
                                 (ExpFun $ VIdent id) `elem` exs]

    -- Calculate the new Map ModName (Map FunName Export) from an old one and
    -- a new module.
    exportMap oldmap name mod = Map.insert name (modExportMap name mod) oldmap

-- | Returns the module name of the given file.
modName :: FilePath -> String
modName = reverse
  . takeWhile (/= '/')
  . tail
  . dropWhile (/= '.')
  . reverse
