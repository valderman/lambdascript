export map, head, tail, take, drop, fst, snd, reverse, filter, otherwise, undefined, length, zipWith, repeat;

data Maybe a = Nothing | Just a;

data Either a b = Left a | Right b;

otherwise :: Bool;
otherwise = True;

undefined :: a;
undefined = error "undefined";

fst :: (a, b) -> a;
fst (a, _) = a;

snd :: (a, b) -> b;
snd (_, b) = b;

head :: [a] -> a;
head (x:_) = x;

tail :: [a] -> [a];
tail (_:xs) = xs;

take :: Int -> [a] -> [a];
take 0 _      = [];
take n (x:xs) = x:take (n-1) xs;

drop :: Int -> [a] -> [a];
drop 0 xs     = xs;
drop n (_:xs) = drop (n-1) xs;
drop _ _      = [];

reverse :: [a] -> [a];
reverse = reverse' [] {
    reverse' acc (x:xs) = reverse' (x:acc) xs;
    reverse' acc _      = acc;
  };

length :: [a] -> Int;
length (x:xs) = 1 + length xs;
length _      = 0;

map :: (a -> b) -> [a] -> [b];
map f (x:xs) = f x : map f xs;
map _ _      = [];

filter :: (a -> Bool) -> [a] -> [a];
filter pred (x:xs) | pred x    = x : filter pred xs;
                   | otherwise = filter pred xs;
filter _ _                     = [];

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c];
zipWith f (a:as) (b:bs) = f a b : zipWith f as bs;
zipWith _ _ _           = [];

repeat :: a -> [a];
repeat x = x : repeat x;
