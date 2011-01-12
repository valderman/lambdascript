import io;

print :: String -> IO Int;
print x = return (_jsfun "(function(x) {print(x); return -1;})" 1 x);

f = print "hej";

main = do { f; f; };
