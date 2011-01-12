import io;

print :: String -> IO ();
print s = case _jsfun "print" 1 s of [] -> return ();;

main = do {
    print "a string!";
    print "another string!";
  };
