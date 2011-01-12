import io;

print :: String -> IO ();
print x = _jsfun "(function(x) {print(x); return 0;})" 1 x;

f = print "hej";

main = do { f; f; };
