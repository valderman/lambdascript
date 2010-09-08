module LambdaScript.TCM where
import LambdaScript.Types

newtype TCM a = TCM {runT :: Int -> Subst -> (a, Int, Subst)}

type Infer e t = [Assump] -> e -> TCM t

instance Monad TCM where
  return x = TCM $ \n s -> (x, n, s)
  m >>= f  = TCM $ \n s -> case runT m n s of
                             (a, n', s') -> runT (f a) n' s'

-- | Create a new type variable with the given kind.
newTVar :: Kind -> TCM AbsType
newTVar k = TCM $ \n s -> (ATVar $ ATyvar (enumId n) k, n+1, s)

-- | Return the current substitution
getSubst :: TCM Subst
getSubst = TCM $ \n s -> (s, n, s)

-- | Extend the current substitution
extSubst :: Subst -> TCM ()
extSubst s' = TCM $ \n s -> ((), n, s' `compose` s)

-- | Instantiate a type scheme; this entails binding a new type variable to
--   each generic, quantified variable of the type.
instantiate :: Scheme -> TCM AbsType
instantiate (Forall ks t) =
  mapM newTVar ks >>= \ts -> return $ inst ts t

-- | Unify two types, extending the global substitution with the resulting
--   substitution.
unify :: AbsType -> AbsType -> TCM ()
unify a b = do
  s <- getSubst
  extSubst =<< mgu (apply s a) (apply s b)

-- | Run a type checking computation.
runTCM :: TCM a -> a
runTCM (TCM f) =
  case f 0 nullSubst of
    (x, n, s) -> x
