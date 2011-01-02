// Generate the lambdas to accept arguments into a constructor. This function
// uses varargs to accomodate optimizations; both of the following are OK:
// _as(3, 0)(arg1, arg2, arg3);
// _as(3, 0)(arg1)(arg2)(arg3);
function _as(n, a) {
  if(n != 0) {
    return function() {
      for(var x = 0; x < arguments.length; ++x)
        a.push(arguments[x]);
      return _as(n-arguments.length, a);
    };
  }
  return a;
}

// Instantiate an algebraic data type.
function _C(args, val) {
  var x = [val];
  return _as(args, x);
}

// Is the given variable undefined?
function _u(x) {
  return (typeof x.x == 'undefined');
}

// Errors are just string exceptions for now.
function error(err) {
  throw err;
}

// Concatenate two lists; this should be moved to the prelude when we
// introduce modules.
function _conc(a, b) {
    if(a()[0]) {
        return [1, a()[1], function _(){if(_u(_)) _.x=_conc(a()[2], b);return _.x;}];
    }
    return b();
}

// Synonym for True; this should also be in the prelude.
function otherwise() {
  return 1;
}
