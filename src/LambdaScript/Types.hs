{-# LANGUAGE FlexibleInstances #-}
module LambdaScript.Types where
import Data.List (foldl', union, intersect)
import LambdaScript.Abs

type ID = String

-- | Abstract types; type var, aggregate type, type constructor and quantified
--   type var.
data AbsType
  = ATVar ATyvar
  | ATApp AbsType AbsType
  | ATCon ATycon
  | ATGen Int
    deriving Eq

-- | Pretty printer for types; tuples are decidedly non-pretty.
instance Show AbsType where
  show (ATVar v) =
    show v
  show (ATApp t1 t2@(ATApp t _)) | t == tArrow =
    show t1 ++ " (" ++ show t2 ++ ")"
  show (ATApp t1 t2) | t1 == tArrow =
    case t2 of
      ATApp _ _ -> "(" ++ show t2 ++ ") " ++ show t1
      _         -> show t2 ++ " " ++ show t1
  show (ATApp t1 t2) | t1 == tList =
    "[" ++ show t2 ++ "]"
  show (ATApp t1 t2) | otherwise =
    show t1 ++ " " ++ show t2
  show (ATCon c) =
    show c
  show (ATGen n) =
    enumId n

-- | Kinds may be either * or Kind -> Kind
data Kind
  = Star
  | KFun Kind Kind
    deriving Eq

-- | Pretty print kinds
instance Show Kind where
  show Star           = "*"
  show (KFun Star k2) = "* -> " ++ show k2
  show (KFun k1 k2)   = "(" ++ show k1 ++ ") -> " ++ show k2

-- | Type variable; an identifier and a kind
data ATyvar = ATyvar ID Kind deriving Eq

instance Show ATyvar where
  show (ATyvar id _) = id

-- | Type constructor; once again identifier + kind
data ATycon = ATycon ID Kind deriving Eq

instance Show ATycon where
  show (ATycon id _) = id

-- | Make an identifier out of an int
enumId :: Int -> ID
enumId n = '*' : show n

-- | Create a kind with arity n.
funKind :: Int -> Kind
funKind 0 = Star
funKind n = KFun Star (funKind $ n-1)

-- | All our basic types.
tUnit, tChar, tInt, tDouble, tList, tArrow, tString :: AbsType
tTuple :: Int -> AbsType
tUnit    = ATCon $ ATycon "()" Star
tChar    = ATCon $ ATycon "Char" Star
tInt     = ATCon $ ATycon "Int" Star
tDouble  = ATCon $ ATycon "Double" Star
tList    = ATCon $ ATycon "[]" (KFun Star Star)
tArrow   = ATCon $ ATycon "->" (KFun Star (KFun Star Star))
tTuple n = ATCon $ ATycon ("(" ++ replicate n ',' ++ ")") (funKind n)
tString  = list tChar

-- | Create an aggregate type.
(~>) :: AbsType -> AbsType -> AbsType
f ~> x   = ATApp (ATApp tArrow f) x
infixr 4 ~>

-- | Create a list of something.
list :: AbsType -> AbsType
list t   = ATApp tList t

-- | Create a tuple from the given types.
tuple :: [AbsType] -> AbsType
tuple ts = foldl' (\x a -> ATApp x a) (tTuple (length ts)) ts

-- | Everything that has a kind; types, for example.
class HasKind t where
  -- | Returns the kind of the given kind-having thing.
  kind :: t -> Kind

instance HasKind ATyvar where
  kind (ATyvar _ k) = k

instance HasKind ATycon where
  kind (ATycon _ k) = k

instance HasKind AbsType where
  kind (ATVar v)   = kind v
  kind (ATCon c)   = kind c
  kind (ATApp a _) = case kind a of (KFun _ k) -> k

-- | Type of substitution.
type Subst = [(ATyvar, AbsType)]

-- | The empty substitution.
nullSubst :: Subst
nullSubst = []

-- | Substitute a variable for a type; a singleton substitution.
(+->) :: ATyvar -> AbsType -> Subst
v +-> t = [(v, t)]

-- | Type-like constructs; this can be, for example, an actual type, a list of
--   types, a substitution, etc.
class Types a where
  -- | Apply a substitution to a type-like.
  apply    :: Subst -> a -> a
  -- | List all free type variables found in a type-like.
  freeVars :: a -> [ATyvar]

instance Types AbsType where
  apply s t@(ATVar v) = case lookup v s of
                          Just t' -> t'
                          _       -> t
  apply s (ATApp a b) = ATApp (apply s a) (apply s b)
  apply s t           = t

  freeVars (ATVar v)   = [v]
  freeVars (ATApp a b) = freeVars a `union` freeVars b
  freeVars _           = []

instance Types a => Types [a] where
  apply s ts  = map (apply s) ts
  freeVars ts = foldr union [] (map freeVars ts)

instance Types (ATyvar, AbsType) where
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
  where allSame = and $ map (\v -> apply s1 (ATVar v) == apply s2 (ATVar v))
                            (map fst s1 `intersect` map fst s2)

-- | Return a substitution representing the binding of the given type variable
--   to the given type. If the kinds of the operands don't match, or if the
--   occurs check fails, we error out.
bind :: Monad m => ATyvar -> AbsType -> m Subst
bind v t | t == ATVar v =
  return nullSubst
         | kind v /= kind t =
  fail $ "Kind mismatch: " ++ show v ++ " of kind " ++ show (kind v) ++ " and "
                           ++ show t ++ " of kind " ++ show (kind t)
         | v `elem` freeVars t =
  fail $ "Occurs check fails: " ++ show v ++ " and " ++ show t
         | otherwise =
  return $ v +-> t

-- | Calculate the most generic unifier of the given types.
mgu :: Monad m => AbsType -> AbsType -> m Subst
mgu (ATApp a1 b1) (ATApp a2 b2) = do
  a <- mgu a1 a2
  b <- mgu (apply a b1) (apply a b2)
  return $ b `compose` a
mgu (ATVar v) t =
  bind v t
mgu t (ATVar v) =
  bind v t
mgu (ATCon c1) (ATCon c2) | c1 == c2 =
  return nullSubst
mgu t1 t2 =
  fail $ "Types don't unify: " ++ show t1 ++ " and " ++ show t2

data Scheme = Forall [Kind] AbsType deriving Eq

instance Types Scheme where
  apply s (Forall ks t) = Forall ks (apply s t)
  freeVars (Forall _ t) = freeVars t

toScheme :: AbsType -> Scheme
toScheme t = Forall [] t

-- | Quantify the given type over the specified type variables.
--   This means that the intersection of the given vars and the free type vars
--   of the given type is "put to rest," so to speak. It's determined once and
--   for all that they're polymorphic, and no longer need concern the outside
--   world.
quantify :: [ATyvar] -> AbsType -> Scheme
quantify vs t = Forall (map kind vs) (apply s t)
  where
    vs' = filter (`elem` vs) (freeVars t)
    s   = zip vs' (map ATGen [0..])

data Assump = ID :>: Scheme

instance Types Assump where
  apply s (id :>: t) = id :>: apply s t
  freeVars (_ :>: t) = freeVars t

-- | Look up a symbol in the table, monadic fail if it's not found.
find :: Monad m => ID -> [Assump] -> m Scheme
find id ((id' :>: sc):as) | id == id' = return sc
                          | otherwise = find id as
find id _ = fail $ "Unbound identifier: " ++ id

-- | Instantiation is basically apply for generic, quantified typevars.
class Instantiate t where
  inst :: [AbsType] -> t -> t

instance Instantiate AbsType where
  inst ts (ATGen n)   = ts !! n
  inst ts (ATApp a b) = ATApp (inst ts a) (inst ts b)
  inst ts t           = t
