map :: (a -> b) -> [a] -> [b];
map f (x:xs)  = f x : map f xs;
map f [x]     = x;
