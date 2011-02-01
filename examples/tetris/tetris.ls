export start;
import dom;
import io;
import canvas;
import std;
import shapes;
import colors;

-- Represents a falling "group" of blocks; the args are:
--  The color of the group
--  The state of the group (index into coordinate list)
--  The block coords of the group, between (0,0) and (9,19)
--  The coordinate list of the individual blocks of the group, relative to the
--  group coords.
data Group = Group Color, Int, (Int, Int), [[(Int, Int)]];

timestep = 250;

-- Make the blocks this size.
blockSize :: Int;
blockSize = 20;

-- Replicate an element n times.
replicate :: Int -> a -> [a];
replicate n x = take n (repeat x);

-- An empty playing field.
emptyField :: [[Maybe Color]];
emptyField = replicate 20 (replicate 10 Nothing);

showInt :: Int -> IO String;
showInt n = _jsfun "(function(x) {return x;})" 1 n;

-- The main game loop.
main :: IORef [[Maybe Color]] -- IORef to the playing field
     -> IORef Group           -- IORef to the falling group
     -> Int                   -- # of lines so far
     -> DOMElement            -- Label to write # of lines to
     -> Canvas                -- Canvas to draw to
     -> IO ();
main fieldref groupref lines linesLbl can = do {
    group <- readIORef groupref;
    field <- readIORef fieldref;
    draw can group field;
    -- Use a case expression here to work around a few nasty bugs with local
    -- definitions.
    case update group field of
      (group', field') -> do {
        grpLines <- case group' of
                     (Just g) -> return (g, lines);
                     _        -> do {
                         nkilledField <- return (killCompleteLines field');
                         writeIORef fieldref (snd nkilledField);
                         g <- newGroup;
                         lk <- return (fst nkilledField);
                         linesTxt <- showInt (lines+lk);
                         setAttr linesLbl "innerHTML" linesTxt;
                         return (g, lines+lk);
                       };
                     ;
        case grpLines of
          (group'', lines) -> do {
              writeIORef groupref group'';
              setTimeout (main fieldref groupref lines linesLbl can)
                         timestep;
            };;
      };;
  };

-- Remove full lines from the playing field.
killCompleteLines :: [[Maybe Color]] -> (Int, [[Maybe Color]]);
killCompleteLines field =
  case fold (\l (xs, n) -> if allJust l then (xs, n+1) else (l:xs, n)) ([], 0) field of
    (xs, n) -> (n, replicate n (replicate 10 Nothing) ++ xs);
    ;

allJust :: [Maybe a] -> Bool;
allJust ((Just _):xs) = allJust xs;
allJust []            = True;
allJust _             = False;

-- Handle key presses
keyHandler :: IORef Group -> IORef [[Maybe Color]] -> Canvas -> Int -> IO ();
keyHandler grp f can k = do {
    grp' <- readIORef grp;
    f' <- readIORef f;
    g <- case grp' of
      (Group c n (x, y) cs)
        | k == 37 -> return (Group c n (x-1,y) cs);
        | k == 39 -> return (Group c n (x+1,y) cs);
        | k == 38 -> return (Group c ((n+1)%length cs) (x,y) cs);
        | otherwise -> return grp';
      ;
    case g of
      (Group c n (x, y) cs) | !(stuck (x, y-1) (get n cs) f') -> do {
        writeIORef grp g;
        draw can g f';
      };
      _ -> return ();
      ;
  };

-- Update the game state.
update :: Group -> [[Maybe Color]] -> (Maybe Group, [[Maybe Color]]);
update (Group c n (x, y) cs) f =
  if stuck (x, y) (get n cs) f
    then (Nothing, toBG c x y (get n cs) f)
    else (Just (Group c n (x, y+1) cs), f)
    ;

-- Merge a falling group of blocks into the background.
toBG :: Color -> Int -> Int -> [(Int, Int)] -> [[Maybe Color]] -> [[Maybe Color]];
toBG c x y cs f = take y f ++ fold (insert2d x (Just c)) (drop y f) cs;

-- Insert a single block into the given two dimensional list.
insert2d :: Int -> a -> (Int, Int) -> [[a]] -> [[a]];
insert2d basex col (x, y) f = put y f (put (basex+x) (get y f) col);

-- A normal right fold.
fold :: (a -> b -> b) -> b -> [a] -> b;
fold f acc (x:xs) = f x (fold f acc xs);
fold _ acc _      = acc;

-- Insert an item into a list at the given position.
put :: Int -> [a] -> a -> [a];
put 0 (_:xs) x'     = x':xs;
put n (x:xs) x' = x:put (n-1) xs x';

-- Was the given block stuck somewhere in the given field?
stuck :: (Int, Int) -> [(Int, Int)] -> [[Maybe Color]] -> Bool;
stuck (bx, by) cs f = (bx < 0)
                      || any (map (\(x, y) -> (y+by > 18) || (x+bx > 9)) cs)
                      || any (map (stuckAt (map (drop bx) (drop by f))) cs);

stuckAt f (x, y) = case get x (get (y+1) f) of
                     (Just _) -> True;
                     _        -> False;
                     ;

any (x:xs) = x || any xs;
any _      = False;

-- Infinite list, increasing by steps of blockSize from 0.
coords :: [Int];
coords = 0 : map (\x -> x+blockSize) coords;

-- Draw the entire playing field.
draw :: Canvas -> Group -> [[Maybe Color]] -> IO ();
draw can grp lines = do {
    sequence_ (zipWith (drawLine can) coords lines);
    drawGroup can grp;
  };

-- Draw a group of falling blocks.
drawGroup :: Canvas -> Group -> IO ();
drawGroup can (Group c n (x, y) cs) = do {
    mapM (\(x', y') -> drawBlock can (blockSize * (y+y'))
                                     (blockSize * (x+x'))
                                     (Just c)) (get n cs);
    return ();
  };

get :: Int -> [a] -> a;
get 0 (x:xs) = x;
get n (x:xs) = get (n-1) xs;
get _ _      = error "Non-exhaustive pattern in function get!";

-- Draw a line of background.
drawLine :: Canvas -> Int -> [Maybe Color] -> IO ();
drawLine can y squares = sequence_ (zipWith (drawBlock can y) coords squares);

-- Draw a single block.
drawBlock :: Canvas -> Int -> Int -> Maybe Color -> IO ();
drawBlock can y x (Just c) = do {
    fillColor can c;
    fillRect can (Pt x y) (Pt blockSize blockSize);
  };
drawBlock can y x _ = do {
    fillColor can black;
    fillRect can (Pt x y) (Pt blockSize blockSize);
  };

-- Start the game.
start :: IO ();
start = do {
    grp <- newGroup;
    grpref <- newIORef grp;
    fieldref <- newIORef emptyField;
    linesLbl <- getElementById "lines";
    withCanvasDo "canvas" (\can -> (do {
          onKeyUp (keyHandler grpref fieldref can);
          case linesLbl of (Just linesLbl') ->
            main fieldref grpref 0 linesLbl' can;
            ;
        }));
  };

newGroup :: IO Group;
newGroup = do {
    x <- random;
    shape <- case x of
               x | x > 0.86  -> return line;
                 | x > 0.71  -> return cube;
                 | x > 0.57  -> return tShape;
                 | x > 0.43  -> return zShape;
                 | x > 0.29  -> return lShape;
                 | x > 0.14  -> return zShapeFlip;
                 | otherwise -> return lShapeFlip;
                 ;
    return (Group pink 0 (4, 0) shape);
  };
