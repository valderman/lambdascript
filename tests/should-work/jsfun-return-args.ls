import io;

test :: IO String;
test = _jsfun "(function() {return 'test';})" 0;

main = do {
    t <- test;
    case t of
      (x:xs) -> return xs;
      _      -> return "FAIL!";
      ;
    };