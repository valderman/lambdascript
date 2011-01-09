import std;

isLeft (Right _) = False;
isLeft _         = True;

main = isLeft (Left "tut");
