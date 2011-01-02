reverse :: [a] -> [a];
reverse = reverse' [] {
    reverse' acc (x:xs) = reverse' (x:acc) xs;
    reverse' acc _      = acc;
  };

concat :: [[a]] -> [a];
concat (x:xs) = x ++ concat xs;
concat _      = [];

sum (x:xs) = x + sum xs;
sum _      = 0;

take 0 _ = [];
take n (x:xs) = x : take (n-1) xs;
take _ _ = [];

main = sum (take 3 (concat [[6, 7, 8], reverse [1, 2, 3, undefined]]));
