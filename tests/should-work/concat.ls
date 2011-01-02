sum (x:xs) = x + sum xs;
sum _      = 0;

main = sum ([1,2] ++ [3,4]);
