import io;

print :: String -> IO ();
print x = case _jsfun "print" 1 x of [] -> return ();;

main = print "a string!";
