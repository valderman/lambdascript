map :: (a -> b) -> [a] -> [b];
map f (x:xs) = f x : map f xs;

get 0 (x:xs) = x;
get n (x:xs) = get (n-1) xs;
get _ _      = error "Index too big!";

main = get 3 nums + get 4 nums {
    nums        = map (\f -> f 10) multipliers;
    multipliers = map (\x -> \y -> x*y) [1,2,3,4,5];
  };
