module Zero where

count :: Eq a => a -> [a] -> Int
count x = go 0
   where
   go n [] = n
   go n (a:as)
      | x == a = go (succ n) as
      | otherwise = go n as

--count x = length . filter (== x)
