seqBool a b =
  case a of
    (False) -> b;
    _       -> b;
  ;

main = seqBool (_jsfun "print" 1 27) 0;
