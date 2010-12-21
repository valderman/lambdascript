reverse :: [a] -> [a];
reverse = reverse' [] {
    reverse' acc (x:xs) = reverse' (x:acc) xs;
    reverse' acc _      = acc;
  };

concat :: [[a]] -> [a];
concat (x:xs) = x ++ concat xs;
concat _      = [];
