f (Just x) = x;
f _        = error "Not Just!";

main = f (Just 42);
