import io;

print :: String -> IO ();
print x = case _jsfun "(function(x) {print(x);})" 1 x of
            [] -> return ();
            ;

f = print "hej";

main = do { f; f; };
