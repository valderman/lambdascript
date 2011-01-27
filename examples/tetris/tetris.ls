export start;
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

-- Make the blocks this size.
blockSize :: Int;
blockSize = 20;

-- Replicate an element n times.
replicate :: Int -> a -> [a];
replicate n x = take n (repeat x);

-- An empty playing field.
emptyField :: [[Maybe Color]];
emptyField = replicate 20 (replicate 10 Nothing);

-- The main game loop.
main :: [[Maybe Color]] -> Group -> Canvas -> IO ();
main field group can = (do {
    draw can group field;
    group'' <- case group' of
                 (Just g) -> return g;
                 _        -> newGroup;
                 ;
    setTimeout (main field' group'' can) 500;
  }) {
    groupfield = update group field;
    group' = fst groupfield;
    field' = snd groupfield;
  };

update :: Group -> [[Maybe Color]] -> (Maybe Group, [[Maybe Color]]);
update (Group c n (x, y) cs) f =
  (Just (Group c n (x, y+1) cs), f);

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
    withCanvasDo "canvas" (main emptyField grp);
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
