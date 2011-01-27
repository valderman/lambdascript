export start;
import io;
import canvas;
import std;
import shapes;
import colors;

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
    draw can field;
  };

-- Infinite list, increasing by steps of blockSize from 0.
coords :: [Int];
coords = 0 : map (\x -> x+blockSize) coords;

-- Draw the entire playing field.
draw :: Canvas -> [[Maybe Color]] -> IO ();
draw can lines = sequence_ (zipWith (drawLine can) coords lines);

drawLine :: Canvas -> Int -> [Maybe Color] -> IO ();
drawLine can y squares = sequence_ (zipWith (drawBlock can y) coords squares);

drawBlock :: Canvas -> Int -> Int -> Maybe Color -> IO ();
drawBlock can y x (Just c) = do {
    fillColor can c;
    fillRect can (Pt x y) (Pt (x+blockSize) (y+blockSize));
  };
drawBlock can y x _ = do {
    fillColor can black;
    fillRect can (Pt x y) (Pt (x+blockSize) (y+blockSize));
  };

-- Start the game.
start :: IO ();
start = do {
    withCanvasDo "canvas" (main emptyField);
  };
