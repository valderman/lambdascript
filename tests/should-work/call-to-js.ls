seqBool a b =
  case a of
    (False) -> b;
    _       -> b;
  ;

main = seqBool (jsfun "print" 1 27) 0;
