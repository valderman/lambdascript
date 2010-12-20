data Maybe a = Just a | Nothing;

f (Just x) = x;
f _        = error "Not Just!";
