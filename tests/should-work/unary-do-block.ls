import io;

print :: String -> IO Int;
print s = do {
    case _jsfun "print" 1 s of [] -> return 0;;
  };

main = print "a string!";
