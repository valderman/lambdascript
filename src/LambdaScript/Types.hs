{-# LANGUAGE FlexibleInstances #-}
-- | Operations on types, and functions to create representations of the basic
--   types in lambdascript.
module LambdaScript.Types where
import Data.List (foldl', union, intersect)
import LambdaScript.Abs

type ID = String

showSc :: Scheme -> String
showSc (Forall _ t) = showT t

showT :: Type -> String
showT (TApp (TCon (TIdent "*Num")) (TCon t)) =
  case t of
    TIdent "*RealInt" -> "Int"
    _                 -> "Double"
showT (TOp t1 t2)   =
  case t1 of
    TOp t1' t2' ->
      "(" ++ showT t1 ++ ") -> " ++ showT t2
    _ ->
      showT t1 ++ " -> " ++ showT t2
showT (TApp t1 t2)  = showT t1 ++ " (" ++ showT t2 ++ ")"
showT (TTup (t:ts)) = "(" ++ foldl' (\a x -> a++","++showT x) (showT t) ts ++ ")"
showT (TLst t)      = "[" ++ showT t ++ "]"
showT (TGen n)      = "*" ++ show n
showT (TUnt)        = "()"
showT (TCon (TIdent c)) = c
showT (TVar (VIdent v)) = v

-- | Make an identifier out of an int
enumId :: Int -> ID
enumId n = '*' : show n

-- | All our basic types.
tUnit, tChar, tInt, tDouble, tList, tArrow, tString :: Type
tTuple :: Int -> Type
tUnit    = TUnt
tChar    = TCon $ TIdent "Char"
tBool    = TCon $ TIdent "Bool"
tInt     = num  $ TCon $ TIdent "*RealInt"
tDouble  = num  $ TCon $ TIdent "*RealDouble"
tList    = TCon $ TIdent "[]"
tNum     = TCon $ TIdent "*Num"
tArrow   = TCon $ TIdent "->"
tTuple n = TCon $ TIdent ("(" ++ replicate n ',' ++ ")")
tString  = list tChar

-- | Return the arity of the given type. Of course, this only works with
--   concrete types; a will always have arity 0 and Int -> a arity 1, etc.
arity :: Type -> Int
arity (TOp _ t) = 1+arity t
arity _         = 0

-- | Create a type from a type constructor and a list of type vars.
mkADT :: TIdent -> [VIdent] -> Type
mkADT id vs = foldl' TApp (TCon id) (map TVar vs)

-- | Create an IO type.
io :: Type -> Type
io t = TApp (TCon (TIdent "IO")) t

-- | Create an aggregate type.
(~>) :: Type -> Type -> Type
f ~> x = f `TOp` x
infixr 4 ~>

-- | Create a list of something.
list :: Type -> Type
list t   = TLst t

-- | Create a tuple from the given types.
tuple :: [Type] -> Type
tuple ts = TTup ts

-- | Create a numeric type.
num :: Type -> Type
num t = TApp tNum t

-- | Create a type variable from an identifier.
tv :: ID -> Type
tv = TVar . VIdent

-- | Type of substitution.
type Subst = [(VIdent, Type)]

-- | The empty substitution.
nullSubst :: Subst
nullSubst = []

-- | Substitute a variable for a type; a singleton substitution.
(+->) :: VIdent -> Type -> Subst
v +-> t = [(v, t)]

-- | Type-like constructs; this can be, for example, an actual type, a list of
--   types, a substitution, etc.
class Types a where
  -- | Apply a substitution to a type-like.
  apply    :: Subst -> a -> a
  -- | List all free type variables found in a type-like.
  freeVars :: a -> [VIdent]

instance Types Type where
  apply s t@(TVar v)  = case lookup v s of
                          Just t' -> t'
                          _       -> t
  apply s (TApp a b)  = TApp (apply s a) (apply s b)
  apply s (TOp a b)   = TOp (apply s a) (apply s b)
  apply s (TLst t)    = TLst $ apply s t
  apply s (TTup ts)   = TTup $ map (apply s) ts
  apply s t           = t

  freeVars (TVar v)   = [v]
  freeVars (TApp a b) = freeVars a `union` freeVars b
  freeVars (TOp a b)  = freeVars a `union` freeVars b
  freeVars (TLst t)   = freeVars t
  freeVars (TTup ts)  = foldr union [] (map freeVars ts)
  freeVars _          = []

instance Types a => Types [a] where
  apply s ts  = map (apply s) ts
  freeVars ts = foldr union [] (map freeVars ts)

instance Types (VIdent, Type) where
  apply s (v, t)  = (v, apply s t)
  freeVars (_, t) = freeVars t

-- | Compose two substitutions, the left taking priority over the right in the
--   event of a conflict.
compose :: Subst -> Subst -> Subst
compose s1 s2 =
  map (apply s1) s2 ++ s1

-- | Monadic merge of two substitutions, erroring out if the substitutions
--   disagree about the type of any variable.
merge :: Monad m => Subst -> Subst -> m Subst
merge s1 s2 =
  if allSame
     then return $ s1 `union` s2
     else fail $  "Merge failed: "
               ++ show s1
               ++ " and "
               ++ show s2
  where allSame = and $ map (\v -> apply s1 (TVar v) == apply s2 (TVar v))
                            (map fst s1 `intersect` map fst s2)

-- | Return a substitution representing the binding of the given type variable
--   to the given type. If the kinds of the operands don't match, or if the
--   occurs check fails, we error out.
bind :: Monad m => VIdent -> Type -> m Subst
bind v t | t == TVar v =
  return nullSubst
         | v `elem` freeVars t =
  fail $ "Occurs check fails: " ++ showT (TVar v) ++ " and " ++ showT t
         | otherwise =
  return $ v +-> t

-- | Calculate the most generic unifier of the given types.
mgu :: Monad m => Type -> Type -> m Subst
mgu (TApp a1 b1) (TApp a2 b2) = do
  a <- mgu a1 a2
  b <- mgu (apply a b1) (apply a b2)
  return $ b `compose` a
mgu (TOp a1 b1) (TOp a2 b2) = do
  a <- mgu a1 a2
  b <- mgu (apply a b1) (apply a b2)
  return $ b `compose` a
mgu (TLst t1) (TLst t2) = do
  mgu t1 t2
mgu (TTup ts1) (TTup ts2) = do
  ss <- sequence $ zipWith mgu ts1 ts2
  return $ foldr union [] ss
mgu (TVar v) t =
  bind v t
mgu t (TVar v) =
  bind v t
mgu (TCon c1) (TCon c2) | c1 == c2 =
  return nullSubst
mgu (TUnt) (TUnt) =
  return nullSubst
mgu t1 t2 =
  fail $ "Types don't unify: " ++ showT t1 ++ " and " ++ showT t2

-- | A type scheme consists of a number of type vars (the first constructor)
--   and a type containing (at most) that many quantified type variables,
--   represented by TGen type constructors.
data Scheme = Forall Int Type deriving (Show, Eq)

instance Types Scheme where
  apply s (Forall n t)  = Forall n (apply s t)
  freeVars (Forall n t) = freeVars t

-- | Creates a type scheme from a type, without quantifying over any type vars.
--   This is useful when we need a type scheme for an assumption but we don't
--   want any of its type vars to be polymorphic.
toScheme :: Type -> Scheme
toScheme t = Forall 0 t

-- | Quantify the given type over the specified type variables.
--   This means that the intersection of the given vars and the free type vars
--   of the given type is "put to rest," so to speak. It's determined once and
--   for all that they're polymorphic, and no longer need concern the outside
--   world.
quantify :: [VIdent] -> Type -> Scheme
quantify vs t = Forall (length vs) (apply s t)
  where
    vs' = filter (`elem` vs) (freeVars t)
    s   = zip vs' (map TGen [0..])

-- | Quantify a type over all of its type variables. This is useful for
--   situations when we know that the given type is final (that is, after
--   completion of type inference.)
quantifyAll :: Type -> Scheme
quantifyAll t = quantify (allVars t) t
  where
    allVars (TVar v)   = [v]
    allVars (TLst t)   = allVars t
    allVars (TTup ts)  = foldl' union [] (map allVars ts)
    allVars (TApp t u) = allVars t `union` allVars u
    allVars (TOp t u)  = allVars t `union` allVars u
    allVars _          = []

data Assump = ID :>: Scheme deriving (Show, Eq)

type Assumps = [Assump]

instance Types Assump where
  apply s (id :>: t) = id :>: apply s t
  freeVars (_ :>: t) = freeVars t

-- | Look up a symbol in the table, monadic fail if it's not found.
find :: Monad m => ID -> Assumps -> m Scheme
find id ((id' :>: sc):as) | id == id' = return sc
                          | otherwise = find id as
find id _ = fail $ "Unbound identifier: " ++ id

-- | Instantiation is basically apply for generic, quantified typevars.
class Instantiate t where
  inst :: [Type] -> t -> t

instance Instantiate Type where
  inst ts (TGen n)   = ts !! fromInteger n
  inst ts (TApp a b) = TApp (inst ts a) (inst ts b)
  inst ts (TOp a b)  = TOp (inst ts a) (inst ts b)
  inst ts (TLst t)   = TLst (inst ts t)
  inst ts (TTup tts) = TTup (map (inst ts) tts)
  inst ts t          = t
