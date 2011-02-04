import io;

print1 x = do {
    s <- _jsfun "(function(x) {print(x); return 0;})" 1 x;  
    return ();
  }; 

print2 x = do {
    _jsfun "(function(x) {print(x); return 0;})" 1 x;  
    return ();
  }; 

printHej = print1 "printHej _";
printHejIO = print1 "printHej IO _";

main = do {
    case printHej of _ -> return ();;
    case print1 "print1 _" of _ -> return ();;
    case print2 "print2 _" of _ -> return ();;
    return "OK";
  };
