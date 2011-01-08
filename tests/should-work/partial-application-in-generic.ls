f :: (a -> b) -> a -> b;
f g x = g x;

g a b = a+b;

main = (f g 10) 20;
