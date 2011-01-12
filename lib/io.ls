export return, alert, setTimeout, mapM;

-- Opaque data type representing a javascript funvtion.
data JSFun = JSFun;

-- IO data constructor.
data IO a = IO a;

-- Monadic return. For highly monomorphic values of monad.
return :: a -> IO a;
return = IO;

-- Javascript alert.
alert :: a -> IO ();
alert s = case _jsfun "alert" 1 s of [] -> return ();;

-- Javascript window.setTimeout.
setTimeout :: IO () -> Int -> IO ();
setTimeout f n = do {
    case _jsfun "window.setTimeout" 2 (_export 0 f) n of
      [] -> return ();;
  };

mapM :: (a -> IO b) -> [a] -> IO [b];
mapM f (x:xs) = do {
    x' <- f x;
    xs' <- mapM f xs;
    return (x':xs');
  };
mapM _ _ = return [];
