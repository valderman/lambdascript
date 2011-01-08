data Foo = Foo Int;

bar :: Foo -> Int;
bar (Foo n) = n+1;

main = bar (Foo 10);
