import std;

main = _jsfun "function(f, x) {return f(x);}" 2 (_export (map (\x -> x*x))) [1,2,3,4,5];
