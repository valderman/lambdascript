-- a binary tree using Ints as its key
data Tree a
  = Nil
  | Tree Int a (Tree a) (Tree a)
  ;

-- an empty tere
empty :: (Tree a);
empty = Nil;

-- search for a key in a tree
find :: Int -> (Tree v) -> (Maybe v);
find x (Nil)   = Nothing ;
find x (Tree k v l r)
  | x > k     = find x r;
  | x < k     = find x l;
  | otherwise = Just v;

-- inserts a value into the tree
insert :: (Int, v) -> (Tree v) -> (Tree v);
insert (k', v') (Tree k v l r)
  | k' > k    = Tree k v l (insert (k', v'));
  | k' < k    = Tree k v (insert (k', v') r);
  | otherwise = Tree k v' l r;
insert (k', v') (Nil)
              = Tree k' v' Nil Nil;

