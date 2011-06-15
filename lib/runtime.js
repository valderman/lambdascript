// Generate lambdas for accepting arguments into a constructor.
function _as(n, a) {
  if(n != 0) {
    return function(x) {
      a.push(x);
      return _as(n-1, a);
    };
  }
  return a;
}

// Trampoline; when TCE is used, all functions are entered through the
// trampoline.
function _t(f, as, thisptr) {
    var r = {f:f,a:as};
    for(;;) {
        r = r.f.apply(thisptr, r.a);
        if(!r.f) {
            return r;
        }
    }
}

// Instantiate an algebraic data type.
function _C(args, val) {
  var x = [val];
  return _as(args, x);
}

// Define a shorthand for undefined, to cut down on code size.
$u = undefined;

// Errors are just string exceptions for now.
function error(err) {
  throw ('User error: ' + _tostr(err));
}

// Turn a normal JS function into something that can be called using the
// LS calling convention. f(a,b,c) becomes f(a)(b)(c) with marshalled args.
function _jsfun(f) {
  return function(n) {return _jsf(eval(_tostr(f)), n.e ? n.x : n.x(), []);};
}

// Allows LS to call a JS function without marshalling any data.
function _rawjsfun(f) {
  return f;
}

// Inlined version of _jsfun; called as $jsfun(f, a1, a2, ...) instead of
// $jsfun(f)(arity)(a1)(a2)..., and takes its argument as an actual JS function
// expression, not an LS list-of-char string.
function $jsfun(f) {
  var a = [];
  for(var i = 1; i < arguments.length; ++i) {
    a.push(_ls2js(arguments[i]));
  }
  return [-1, {x:function() {return _js2ls(f.apply(null, a)).x;}}];
}

function _jsf(f, n, as) {
  if(as.length == n) {
    return [-1, {x:function() {return _js2ls(f.apply(null, a)).x;}}];
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
    return {e:1, x:_s(x)};
  }
  if(typeof x == 'undefined') {
    return {e:1, x:[0]};
  }
  return {e:1, x:x};
}

// Marshal arguments from LS to JS.
function _ls2js(x) {
  if(typeof x == 'function') {
    x = _t(x, []);
  } else {
    x = x.e ? x.x : x.x();
  }

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
    if((x[1].e ? x[1].x : x[1].x()).charAt) {
      // A [Char]; that's a string.
      return _tostr({x:function() {return x;}});
    } else {
      var s = [];
      // Something else; that's an array then.
      while(x[0]) {
        s.push(_ls2js(x[1]));
        x = x[2].e ? x[2].x : x[2].x();
      }
      return s;
    }
  } else if(x.x !== undefined) {
    // Apparently it's a thunk.
    return _ls2js(x);
  }
  return x;
}

// Concatenate two lists; this should be moved to the prelude when we
// introduce modules.
function $conc(a, b) {
  a = a.e ? a.x : a.x();
  if(a[0]) {
      return [1, a[1], {x:function(){this.e=1;this.x=$conc(a[2], b);return this.x;}}];
  }
  return b.x();
}

// Turn a JS string literal into a list. The list is not lazy, since we have
// better uses for the call stack than to lazify an already finite, strict
// string of characters.
function _s(s) {
  var l = [0];
  function x(i) {
    return {x:function() {return s[i];}};
  }
  function xs(l) {
    return {x:function() {return l;}};
  }
  for(var i = s.length-1; i >= 0; --i) {
    l = [1, x(i), xs(l)];
  }
  return l;
}

// Turn a LS list-string into a JS "normal" string.
function _tostr(xs) {
  var s = '';
  var x;
  while((x = (xs.e ? xs.x : xs.x()))[0]) {
    s += x[1].e ? x[1].x : x[1].x();
    xs = x[2];
  }
  return s;
}

// LS-callable variant of _exp.
function _export(n) {
  return function(f) {
      f = f.e ? f.x : f.x();
      return _exp((n.e ? n.x : n.x()) ? f : function() {return f;});
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
  if(a[0].x) {
    var x = a[0].e ? a[0].x : a[0].x();
    var y = b[0].e ? b[0].x : b[0].x();
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
    var res = $cmp(a[i].e ? a[i].x : a[i].x(),
                   b[i].e ? b[i].x : b[i].x());
    if(res != 0) {
      return res;
    }
  }
  return 0;
}

function $newIORef(x) {
  var ref = new Object();
  ref.val = x;
    return [-1, {e:1, x:ref}];
}

function $readIORef(ref) {
  return [-1, (ref.e ? ref.x : ref.x()).val];
}

function $writeIORef(ref) {
    return function(x) {
        (ref.e ? ref.x : ref.x()).val = x;
        return [-1, {e:1, x:[0]}];
    };
}

function $onKeyUp(f) {
  document.onkeyup = function(ev) {f(ev.which)};
  return 0;
}

function $bind(m) {
  return function(f) {
      return [-1, {x: function(){
          m = m.e ? m.x : _t(m.x, [], m);
        m = m[1].e ? m[1].x : m[1].x();
        var x = _t(f.e ? f.x : f.x(), [{e:1, x:m}])[1];
        return x.e ? x.x : x.x();
      }}];
  }
}

function div(a) {
  return function(b) {return Math.floor(a()/b());};
}
