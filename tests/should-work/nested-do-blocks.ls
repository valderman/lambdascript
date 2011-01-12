import io;

main = do {
    do {
      x <- return 5;
      y <- return 5;
      return (10 + x + y);
    };
  };
