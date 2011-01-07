data List a = Cons a, (List a) | Nil;

f (Cons x (Cons y (Cons z _))) = 42;
f _                            = error "Data constructor matching fails!";

main = f (Cons 0 (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))));
