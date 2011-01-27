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
main :: [[Maybe Color]] -> Canvas -> IO ();
main field can = do {
    draw can (Group pink 0 (3, 3) line) field;
  };

-- Infinite list, increasing by steps of blockSize from 0.
coords :: [Int];
coords = 0 : map (\x -> x+blockSize) coords;

-- Draw the entire playing field.
draw :: Canvas -> Group -> [[Maybe Color]] -> IO ();
draw can grp lines = do {
    sequence_ (zipWith (drawLine can) coords lines);
    drawGroup can grp;
  };

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

drawLine :: Canvas -> Int -> [Maybe Color] -> IO ();
drawLine can y squares = sequence_ (zipWith (drawBlock can y) coords squares);

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
    withCanvasDo "canvas" (main emptyField);
  };
