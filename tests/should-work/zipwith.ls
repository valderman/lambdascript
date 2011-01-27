import std;

sum (x:xs) = x + sum xs;
sum _      = 0;

main = sum (zipWith (\a b -> a+b) [1,2,3] [3,2,1]);
