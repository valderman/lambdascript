import io;

init = case _jsfun "(function() {herpderp = 0; return 0;})" 0 of 0 -> return ();;
plusEtt = case _jsfun "(function() {herpderp++; return herpderp;})" 0 of x -> return x;;

plus n = case _jsfun "(function(x) {herpderp+=x; return herpderp;})" 1 n of x -> return x;;

plusTva = plus 2;

main = do {
    init;
    x <- plusEtt;
    y <- plusEtt;
    z <- plusEtt;
    plusEtt;
    plusTva;
    plusEtt;
  };
