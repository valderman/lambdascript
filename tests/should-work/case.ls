data SomeType
  = A
  | B
  | C
  | D Int
  ;

showSomeType t =
  case t of
    (A)   -> "A";
    (B)   -> "B";
    (C)   -> "C";
    (D n) -> if n < 0 then "D negative" else "D positive";
  ;

main = showSomeType A ++ showSomeType (D 10);
