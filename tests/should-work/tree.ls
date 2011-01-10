import std;

-- a binary tree using Ints as its key
data Tree a
  = Nil
  | Tree Int, a, Tree a, Tree a
  ;

-- an empty tree
empty :: Tree a;
empty = Nil;


-- search for a key in a tree
find :: Int -> Tree v -> Maybe v;
find x (Nil)   = Nothing ;
find x (Tree k v l r)
  | x > k     = find x r;
  | x < k     = find x l;
  | otherwise = Just v;


-- inserts a value into the tree
insert :: (Int, v) -> Tree v -> Tree v;
insert (k', v') (Tree k v l r)
  | k' > k    = Tree k v l (insert (k', v') r);
  | k' < k    = Tree k v (insert (k', v') l) r;
  | otherwise = Tree k v' l r;
insert (k', v') (Nil)
              = Tree k' v' Nil Nil;

t = insert (10, 10) (insert (5, 5) (insert (15, 15) (insert (25, 25) (insert (3, 3) empty))));

main =
  case find 15 t of
    (Just n) -> case find 3 t of
                  (Just n') -> case find 26 t of
                                 (Just _)  -> error "Nonexistent element 26 found!";
                                 (Nothing) -> n+n';
                               ;
                  _         -> error "Element 3 not found!";
                ;
    _        -> error "Element 15 not found!";
  ;
