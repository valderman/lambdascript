export withCanvasDo, getCanvas, fillColor, fillRect, Canvas, Color(..), Point(..);
import io;
import dom;
import std;

-- Abstract data type representing a canvas object.
data Canvas = Canvas DOMElement;

-- Represent colors!
data Color = RGB Int, Int, Int;

-- Represent points!
data Point = Pt Int,Int;

-- Fetches a canvas based on its ID property.
getCanvas :: String -> IO (Maybe Canvas);
getCanvas id = do {
    x <- getElementById id;
    case x of
      (Just x') -> do {
          n <- getAttr x' "tagName";
          if n == "CANVAS"
            then do {
                ctx <- _jsfun "(function(x) {return x.getContext('2d');})" 1 x';
                valid <- domElemValid ctx;
                if valid then return (Just (Canvas ctx)) else return Nothing;
              }
            else return Nothing;
        };

      _ -> return Nothing;
      ;
  };

-- Set the fill color for the canvas' context.
fillColor :: Canvas -> Color -> IO ();
fillColor (Canvas can) (RGB r g b) = do {
    _jsfun "(function (c,r,g,b) {c.fillStyle = 'rgb('+r+','+g+','+b+')'; return 0;})" 4 can r g b;
    return ();
  };

-- Fill a rectangle using the current fill color.
fillRect :: Canvas -> Point -> Point -> IO ();
fillRect (Canvas can) (Pt x1 y1) (Pt x2 y2) = do {
    _jsfun "(function (c,x1,y1,x2,y2) {c.fillRect(x1,y1,x2,y2); return 0;})" 5 can x1 y1 x2 y2;
    return ();
  };

-- Attempt to acquire the canvas with the given name; if that fails, throw an
-- error. If it succeeds, run the given action.
withCanvasDo :: String -> (Canvas -> IO a) -> IO a;
withCanvasDo can f = do {
    can' <- getCanvas can;
    case can' of
      (Just c) -> f c;
      _        -> error ("Failed to acquire canvas " ++ can ++ "!");
      ;
  };
