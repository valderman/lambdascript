-- This should actually work, since bindings shadowing each other is perfectly
-- legal.
f = True;
f = False;

main = f;
