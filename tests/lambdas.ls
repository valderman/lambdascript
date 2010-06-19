map :: (a -> b) -> [a] -> [b]
map f (x:xs) = f x : map f xs;

zip :: [a] -> [b] -> [(a, b)]
zip (x:xs) (y:ys) = (x, y) : zip xs ys;
zip _ _           = [];

main = (multipliers, pairs) {
    multipliers = map (\x -> \y -> x*y) [1,2,3,4,5];
    pairs = map (\(_, b) -> b) (zip [1,2,3,4,5] "abcde");
  };
