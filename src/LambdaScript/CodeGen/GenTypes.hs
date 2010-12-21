-- | Compiles user-defined types into numeric representations.
module LambdaScript.CodeGen.GenTypes where
import LambdaScript.Abs
import LambdaScript.CodeGen.Ops
import qualified Data.Map as M
import Data.List (foldl')

-- | Generate the constructor -> ID mapping for a typedef.
typeMap :: NewType -> M.Map String ConstrID
typeMap (NewType _ _ cs) =
  M.fromList $ zip (map (\(Constructor (TIdent id) _) -> id) cs) [0..]

-- | Generate the constructor -> ID mapping for all typedefs.
allTypesMap :: Program -> M.Map String ConstrID
allTypesMap (Program defs) =
  foldl' addConstrs M.empty defs
  where
    addConstrs m (TypeDef nt) = m `M.union` typeMap nt
    addConstrs m _            = m
