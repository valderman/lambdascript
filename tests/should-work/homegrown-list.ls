data List a = Cons a, (List a) | Nil;

f (Cons x xs) = x;

main = f (Cons 9 undefined);
