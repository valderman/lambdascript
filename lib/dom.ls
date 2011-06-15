-- This is basically a wrapper for the more useful DOM manipulation functions.
export getElementById, parentNode, getAttr, setAttr, firstChild, lastChild,
       nextSibling, prevSibling, childNodes, siblings, domElemValid,
       DOMElement, withElement, updateAttr;
import io;
import std;

data DOMElement = DOMElement;

-- Checks whether the given value, received from JS, is not null.
domElemValid :: a -> IO Bool;
domElemValid x = _jsfun "(function(x) {return x;})" 1 x;

-- document.getElementById wrapper
getElementById :: String -> IO (Maybe DOMElement);
getElementById id = do {
      x <- _jsfun "(function (id) {var x = document.getElementById(id); return x ? x : 0;})" 1 id;
      valid <- domElemValid x;
      if valid then return (Just x) else return Nothing;
  };

-- Performs an IO action using the element with the given ID.
-- If the element doesn't exist, Nothing is returned.
withElement :: String -> (DOMElement -> IO a) -> IO (Maybe a);
withElement id m = do {
    e <- getElementById id;
    case e of
      (Just e) -> do {x <- m e; return (Just x);};
      _        -> return Nothing;
      ;
  };

-- Reads any DOM node text attribute.
getAttr :: DOMElement -> String -> IO String;
getAttr e attr = _jsfun "(function(e,s) {return e[s];})" 2 e attr;

-- Writes any DOM node text attribute.
setAttr :: DOMElement -> String -> String -> IO ();
setAttr e attr s = do {
    _jsfun "(function(e,attr,s) {e[attr] = s;})" 3 e attr s;
    return ();
  };

-- Applies the given function to the value of the given attribute of the given
-- element and updates that attribute with the result. If the element doesn't
-- exist, the function does nothing.
updateAttr :: DOMElement -> String -> (String -> String) -> IO ();
updateAttr e a f = do {
    s <- getAttr e a;
    setAttr e a (f s);
  };

-- Wrapper for <element>.parentNode
parentNode :: DOMElement -> IO DOMElement;
parentNode e = do {
    x <- _jsfun "(function(e) {return e.parentNode;})" 1 e;
    x' <- domElemValid x;
    if x' then return x else error "parentNode: element has no parent!";
  };

-- Wrapper for <element>.nextSibling
nextSibling :: DOMElement -> IO (Maybe DOMElement);
nextSibling e = do {
    x <- _jsfun "(function(e) {return e.nextSibling;})" 1 e;
    x' <- domElemValid x;
    if x' then return (Just x) else return Nothing;
  };

-- Wrapper for <element>.previousSibling
prevSibling :: DOMElement -> IO (Maybe DOMElement);
prevSibling e = do {
    x <- _jsfun "(function(e) {return e.previousSibling;})" 1 e;
    x' <- domElemValid x;
    if x' then return (Just x) else return Nothing;
  };

-- Wrapper for <element>.firstChild
firstChild :: DOMElement -> IO (Maybe DOMElement);
firstChild e = do {
    x <- _jsfun "(function(e) {return e.firstChild;})" 1 e;
    x' <- domElemValid x;
    if x' then return (Just x) else return Nothing;
  };

-- Wrapper for <element>.lastChild
lastChild :: DOMElement -> IO (Maybe DOMElement);
lastChild e = do {
    x <- _jsfun "(function(e) {return e.lastChild;})" 1 e;
    x' <- domElemValid x;
    if x' then return (Just x) else return Nothing;
  };

-- Wrapper for <element>.childNodes
childNodes :: DOMElement -> IO [DOMElement];
childNodes e = _jsfun "(function(e) {return e.childNodes;})" 1 e;

-- Wrapper for <element>.childNodes
siblings :: DOMElement -> IO [DOMElement];
siblings e = _jsfun "(function(e) {return e.siblings;})" 1 e;
