import std;

down 0 = [0];
down n = n : down (n-1);

main = head (drop 5 (map (\x -> x*x) (down 10)));
