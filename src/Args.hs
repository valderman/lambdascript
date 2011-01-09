-- | Functions for handling command line arguments in a somewhat orderly
--   fashion.
module Args (Handled (..), Handler, match, (==>), startsWith, withParams) where
import Data.List (foldl')

type Handler a = String -> [String] -> a -> Handled a

-- | Match arguments against predicates, applying a transformation to a config
--   whenever the predicate matches.
match :: a -> [String] -> [Handler a] -> a
match cfg args handlers =
  fst $ foldl' (\(a, (_:args)) arg ->
                 (handle handlers a arg args, args))
               (cfg, args) args

data Handled a = Ok a | Continue a | NotMine

-- | Iterate from left to right over a list of handlers. The first handler that
--   handles the argument returns Ok <updated argument> which breaks out of the
--   loop. Any handler that doesn't handle the argument just returns NotMine,
--   to let the search for a new handler continue.
handle :: [Handler a] -- ^ List of handlers to try on the argument.
       -> a           -- ^ Initial value to be updated on successful handling.
       -> String      -- ^ The argument to be handled
       -> [String]    -- ^ All arguments following the current argument.
       -> a
handle (f:fs) a arg args =
  case f arg args a of
    Ok a'       -> a'
    Continue a' -> a' `seq` handle fs a' arg args
    _           -> handle fs a arg args
handle _ a _ _ =
  a

-- | Pair predicates with transformations.
(==>) :: (String -> Bool) -> Handler a -> Handler a
(==>) pred act = \s ss a -> if pred s then act s ss a else NotMine

-- | Matches whenever the string to match starts with the given prefix.
startsWith :: String -> String -> Bool
startsWith s = (== s) . take (length s)

-- | Split a list on each occurrence of a delimiter.
splitBy :: Eq a => a -> [a] -> [[a]]
splitBy delim = splitBy' []
  where
    splitBy' a (x:xs)
      | x == delim =
        reverse a : splitBy' [] xs
      | otherwise =
        splitBy' (x:a) xs
    splitBy' a _ =
      [reverse a]

-- | "Parses" the parameters of an argument for a handler.
--   The parameters are assumed to be specified either like -xfoo,bar or like
--   -m foo,bar.
withParams :: Int -> ([String] -> a -> Handled a) -> Handler a
withParams len h s ss a =
  case drop len s of
    "" -> h (splitBy ',' $ head ss) a
    s' -> h (splitBy ',' s') a
