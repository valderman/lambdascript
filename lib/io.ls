export return, alert, setTimeout, mapM, random, sequence_, newIORef, readIORef, writeIORef, onKeyUp, clearKeyUp;

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
alert s = do {
    _jsfun "alert" 1 s;
    return ();
  };

-- Javascript window.setTimeout.
setTimeout :: IO () -> Int -> IO ();
setTimeout f n = do {
    _jsfun "window.setTimeout" 2 (_export 0 f) n;
    return ();
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
random = _jsfun "Math.random" 0;

sequence_ :: [IO ()] -> IO ();
sequence_ (x:xs) = do {x; sequence_ xs;};
sequence_ _      = return ();

newIORef :: a -> IO (IORef a);
newIORef x = _rawjsfun "$newIORef" 1 x;

readIORef :: IORef a -> IO a;
readIORef ref = _rawjsfun "$readIORef" 1 ref;

writeIORef :: IORef a -> a -> IO ();
writeIORef ref x = do {
    _rawjsfun "$writeIORef" 2 ref x;
    return ();
  };

onKeyUp :: (Int -> IO ()) -> IO ();
onKeyUp f = do {
    _jsfun "$onKeyUp" 1 (_export 1 f);
    return ();
  };

clearKeyUp :: IO ();
clearKeyUp = do {
    _jsfun "(function() {document.onkeyup = null; return 0;})" 0;
    return ();
  };
