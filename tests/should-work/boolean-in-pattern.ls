import std;

f (True)  = 3;
f (False) = 0;

g (Just (True))  = 0;
g (Just (False)) = 3;

main = f True * g (Just False);
