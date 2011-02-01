main = foo 3 3;

foo a b = a' + b' {
    a' = x+2;
    b' = x-2;
    x = bar a b;
  };

bar a b = a+b;
