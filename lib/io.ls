export return, alert, setTimeout, mapM, random, sequence_, newIORef, readIORef, writeIORef;

-- Opaque data type representing a javascript funvtion.
data JSFun = JSFun;

-- Opaque data type representing an IO reference.
data IORef a = IORef;

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

-- Returns a random double between 0 and 1.
random :: IO Double;
random = case _jsfun "Math.random" 0 of d -> return d;;

sequence_ :: [IO ()] -> IO ();
sequence_ (x:xs) = do {x; sequence_ xs;};
sequence_ _      = return ();

newIORef :: a -> IO (IORef a);
newIORef x =
  case _jsfun "$newIORef" 1 x of
    x' -> return x';
    ;

readIORef :: IORef a -> IO a;
readIORef ref =
  case _jsfun "$readIORef" 1 ref of
    x -> return x;
    ;

writeIORef :: IORef a -> a -> IO ();
writeIORef ref x =
  case _jsfun "$writeIORef" 2 ref x of
    0 -> return ();
    ;