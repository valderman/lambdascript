import io;

print x = case _jsfun "print" 1 x of [] -> return ();;

main = sequence_ [
    print "this",
    print "is",
    print "a",
    print "sequence"
  ];
