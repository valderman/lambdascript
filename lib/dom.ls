-- This is basically a wrapper for the more useful DOM manipulation functions.
export getElementById, parentNode, getAttr, setAttr, firstChild, lastChild,
       nextSibling, prevSibling, childNodes, siblings, domElemValid;
import io;
import std;

data DOMElement = DOMElement;

-- Checks whether the given value, received from JS, is not null.
domElemValid :: a -> Bool;
domElemValid x = _jsfun "(function(x) {return x;})" 1 x;

-- document.getElementById wrapper
getElementById :: String -> IO (Maybe DOMElement);
getElementById id = do {
      case _jsfun "(function (id) {var x = document.getElementById(id); return x ? x : 0;})" 1 id of
        x | domElemValid x -> return (Just x);
          | otherwise      -> return Nothing;
        ;
  };

-- Reads any DOM node text attribute.
getAttr :: DOMElement -> String -> IO String;
getAttr e attr = do {
    case _jsfun "(function(e,s) {return e[s];})" 2 e attr of
      (c:s) -> return (c:s);
      []    -> return [];
      ;
  };

-- Writes any DOM node text attribute.
setAttr :: DOMElement -> String -> String -> IO ();
setAttr e attr s = do {
    case _jsfun "(function(e,attr,s) {e[attr] = s;})" 3 e attr s of [] -> return ();;
  };


-- Wrapper for <element>.parentNode
parentNode :: DOMElement -> IO DOMElement;
parentNode e =
  case _jsfun "(function(e) {return e.parentNode;})" 1 e of
    x | domElemValid x -> return x;
    _                  -> error "parentNode: element has no parent!";
    ;

-- Wrapper for <element>.nextSibling
nextSibling :: DOMElement -> IO (Maybe DOMElement);
nextSibling e =
  case _jsfun "(function(e) {return e.nextSibling;})" 1 e of
    x | domElemValid x -> return (Just x);
    _                  -> return Nothing;
    ;

-- Wrapper for <element>.previousSibling
prevSibling :: DOMElement -> IO (Maybe DOMElement);
prevSibling e =
  case _jsfun "(function(e) {return e.previousSibling;})" 1 e of
    x | domElemValid x -> return (Just x);
    _                  -> return Nothing;
    ;

-- Wrapper for <element>.firstChild
firstChild :: DOMElement -> IO (Maybe DOMElement);
firstChild e =
  case _jsfun "(function(e) {return e.firstChild;})" 1 e of
    x | domElemValid x -> return (Just x);
    _                  -> return Nothing;
    ;

-- Wrapper for <element>.lastChild
lastChild :: DOMElement -> IO (Maybe DOMElement);
lastChild e =
  case _jsfun "(function(e) {return e.lastChild;})" 1 e of
    x | domElemValid x -> return (Just x);
    _                  -> return Nothing;
    ;

-- Wrapper for <element>.childNodes
childNodes :: DOMElement -> IO [DOMElement];
childNodes e =
  case _jsfun "(function(e) {return e.childNodes;})" 1 e of
    [] -> return [];
    xs -> return xs;
    ;

-- Wrapper for <element>.childNodes
siblings :: DOMElement -> IO [DOMElement];
siblings e =
  case _jsfun "(function(e) {return e.siblings;})" 1 e of
    [] -> return [];
    xs -> return xs;
    ;
