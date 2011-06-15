-- Scrolls the text inside an HTML element to the left.
export scroll;
import io;
import std;
import dom;

update t e = do {
    updateAttr e "innerHTML" (\txt -> tail txt ++ [head txt]);
    setTimeout (update t e) t;
  };

scroll id t = withElement id (update t);
