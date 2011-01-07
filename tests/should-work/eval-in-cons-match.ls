f (x:y:_) = 42;
f _     = error "Cons matching fails!";

main = f [1,2,3,4,5];
