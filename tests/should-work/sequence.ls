import io;

print x = do {
    _jsfun "(function(x) {print(x);})" 1 x;  
    return ();
  }; 

main = sequence_ [
    print "this",
    print "is",
    print "a",
    print "sequence"
  ];
