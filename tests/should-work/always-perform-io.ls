import io;

print :: String -> IO a;
print x = _jsfun "(function(x) {print(x); return 0;})" 1 x;

f = print "hej";

main = do { f; f; };
