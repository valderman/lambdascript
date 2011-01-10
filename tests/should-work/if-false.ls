f = error "halp";

g x y = if x then y else 10;

main = g False f;
