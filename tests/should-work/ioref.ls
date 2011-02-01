import io;

doSomethingTo x = do {
    x' <- readIORef x;
    writeIORef x (x'+1332);
  };

main = do {
    x <- newIORef 5;
    doSomethingTo x;
    readIORef x;
  };
