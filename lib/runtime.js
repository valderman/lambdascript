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
  throw ('User error: ' + _tostr(err));
}

// Turn a normal JS function into something that can be called using the
// LS calling convention. f(a,b,c) becomes f(a)(b)(c) with marshalled args.
function _jsfun(f) {
  return function(n) {return _jsf(eval(_tostr(f)), n(), []);};
}

// Inlined version of _jsfun; called as $jsfun(f, a1, a2, ...) instead of
// $jsfun(f)(arity)(a1)(a2)..., and takes its argument as a JS string (not an
// LS list-of-char string).
function $jsfun(f) {
  f = eval(f);
  var a = [];
  for(var i = 1; i < arguments.length; ++i) {
    a.push(_ls2js(arguments[i]));
  }
  return _js2ls(f.apply(null, a))();
}

function _jsf(f, n, as) {
  if(as.length == n) {
    return _js2ls(f.apply(null, as))();
  }
  return function(x) {
      as.push(_ls2js(x));
      return _jsf(f, n, as);
    };
}

if(typeof NodeList == 'undefined') {
  function NodeList() {}
}

// Marshal arguments from JS to LS.
function _js2ls(x) {
  if(x && (x instanceof Array || x.charAt || x instanceof NodeList)) {
    return function() {return _s(x);};
  }
  if(typeof x == 'undefined') {
    return function() {return [0];};
  }
  return function() {return x;};
}

// Marshal arguments from LS to JS.
function _ls2js(x) {
  x = x();
  // Lists get converted into either arrays or strings.
  if(x instanceof Array) {
    // If the length is one, then it's a nullary constructor.
    if(x.length == 1) {
      return [];
    }

    // If the constructor is -1, then it's IO!
    if(x[0] == -1) {
      return _ls2js(x[1]);
    }

    // If the constructor's argument is a char, then it's a string!
    if(x[1]().charAt) {
      // A [Char]; that's a string.
      return _tostr(function(){return x;});
    } else {
      var s = [];
      // Something else; that's an array then.
      while(x[0]) {
        s.push(_ls2js(x[1]));
        x = x[2]();
      }
      return s;
    }
  }
  return x;
}

// Concatenate two lists; this should be moved to the prelude when we
// introduce modules.
function $conc(a, b) {
  if(a()[0]) {
    return [1, a()[1], function _(){if(_u(_)) _.x=$conc(a()[2], b);return _.x;}];
  }
  return b();
}

// Turn a JS string literal into a list. The list is not lazy, since we have
// better uses for the call stack than to lazify an already finite, strict
// string of characters.
function _s(s) {
  var l = [0];
  function x(i) {
    return function() {return s[i];}
  }
  function xs(l) {
    return function() {return l;}
  }
  for(var i = s.length-1; i >= 0; --i) {
    l = [1, x(i), xs(l)];
  }
  return l;
}

// Turn a LS list-string into a JS "normal" string.
function _tostr(xs) {
  var s = '';
  while(xs()[0]) {
    s += xs()[1]();
    xs = xs()[2];
  }
  return s;
}

// LS-callable variant of _exp.
function _export(n) {
  return function(f) {
           return _exp(n() ? f() : f);
         };
}

// Export the given function.
function _exp(f) {
  if(f.length == 0) {
    return function() {return _ls2js(f);};
  }
  return function() {
      var res = f;
      for(var i = 0; i < arguments.length; ++i) {
        res = res(_js2ls(arguments[i]));
      }
      return _ls2js(function() {return res;});
    };
}

// Compare two complex values.
function $cmp(a, b) {
  // If a is not an array, then it's not a complex type.
  if(!(a instanceof Array)) {
    if(typeof a == 'string') {
      return a.charCodeAt(0) - b.charCodeAt(0);
    }
    return a - b;
  }
  
  // If a[0] is a function, this is a tuple. If not, it's some other ADT.
  if(a[0] instanceof Function) {
    var x = a[0]();
    var y = b[0]();
    if(x != y) {
      return x - y;
    }
  } else if(a[0] != b[0]) {
    return a[0] - b[0];
  }
  
  // Loop through all the constructor's fields, comparing them.
  // Since both values are guaranteed to be of the same type, both arrays
  // have the same length.
  for(var i = 1; i < a.length; ++i) {
    var res = $cmp(a[i](), b[i]());
    if(res != 0) {
      return res;
    }
  }
  return 0;
}
