export _bind, return, alert, setTimeout;

-- Opaque data type representing a javascript funvtion.
data JSFun = JSFun;

-- IO data constructor.
data IO a = IO a;

-- Monadic bind.
_bind :: IO a -> (a -> IO b) -> IO b;
_bind (IO a) f = f a;

-- Monadic return. For highly monomorphic values of monad.
return :: a -> IO a;
return = IO;

-- Javascript alert.
alert :: a -> IO ();
alert s = _jsfun "alert" 1 s;

-- Javascript window.setTimeout.
setTimeout :: IO () -> Int -> IO ();
setTimeout f n = do {
    _jsfun "window.setTimeout" 2 (_export 0 f) n;
    return ();
  };
