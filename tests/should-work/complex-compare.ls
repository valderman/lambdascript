import std;

-- Equality operator
f = if "test" == "test" then "ok" else "fail";

-- Non-list structures
g = if (Just "test") == (Just "test") then "ok" else "fail";

-- Make sure equality doesn't hold when it shouldn't
h = if "test" == "derp" then "fail" else "ok";

-- Inequality
i = if "test" != "derp" then "ok" else "fail";

-- a > b for data types with different constructors
j = if (Just (0-2)) > Nothing then "ok" else "fail";

-- a > b for lists of different length
k = if [3] > [1,2,3] then "ok" else "fail";
l = if [1,2,3] < [1,2,3,0] then "ok" else "fail";

-- Tuples
m = if (1,2,3) < (3,2,1) then "ok" else "fail";

main = f ++ g ++ h ++ i ++ j ++ k ++ l ++ m;
