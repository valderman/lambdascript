import std;

main = _jsfun "function(f, x) {return f(x);}" 2 (_export 1 (map (\x -> x*x))) [1,2,3,4,5];
