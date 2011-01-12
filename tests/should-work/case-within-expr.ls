f x = 3 + (case x of
            n | n > 5 -> 1000;
              | True  -> 0;)
            ;

main = f 10;
