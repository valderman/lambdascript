import io;

printString :: String -> IO ();
printString s = _jsfun "print" 1 s;

main = printString "a string!";
