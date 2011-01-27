import std;

sum (x:xs) = x + sum xs;
sum _      = 0;

main = sum (take 10 (repeat 5));
