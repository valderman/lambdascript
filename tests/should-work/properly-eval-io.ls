import io;

init = do {_jsfun "(function() {herpderp = 0; return 0;})" 0; return ();};
plusEtt = _jsfun "(function() {herpderp++; return herpderp;})" 0;
plus n = _jsfun "(function(x) {herpderp+=x; return herpderp;})" 1 n;

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
