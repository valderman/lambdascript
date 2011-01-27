import io;

main = do {
    x <- random; y <- random; z <- random;
    if (x > 1.0) || (x < 0.0) ||
       (y > 1.0) || (y < 0.0) ||
       (z > 1.0) || (z < 0.0) ||
       (x == y) || (x == z)
      then return "fail"
      else return "OK!";
  };
