import io;

print :: String -> IO ();
print x = do {
    _jsfun "(function(x) {print(x);})" 1 x;  
    return ();
  };

main = print "a string!";
