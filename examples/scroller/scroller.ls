-- Scrolls the text inside an HTML element to the left.
export scroll;
import io;
import std;
import dom;

update :: DOMElement -> Int -> IO ();
update e t = do {
    txt <- getAttr e "innerHTML";
    setAttr e "innerHTML" (tail txt ++ [head txt]);
    setTimeout (update e t) t;
  };

scroll :: String -> Int -> IO ();
scroll id t = do {
    elem <- getElementById id;
    case elem of
      (Just e) -> update e t;
      _        -> error ("No such element: " ++ id);
      ;
  };
