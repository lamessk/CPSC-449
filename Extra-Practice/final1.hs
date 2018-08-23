okPassword :: String -> Bool 
okPassword pass 
  | lenP >= 8 && 
    length [ n | n <- pass, n >= 'A', n <= 'Z'] > 0, 
    length [ i | i <- pass, i >= 'a', i <= 'z'] > 0, 
    length [ j | j <- pass, j >= '0', j <= '9'] > 0 = True
  | otherwise = False  
  where 
    lenP = length pass 


pzn :: [Int] -> (Int, Int, Int)
pzn list = foldl pznHelper (0,0,0) list

pznHelper :: (Int, Int, Int) -> Int -> (Int, Int, Int) 
pznHelper (p, z, n) a
  | a >  0 = (p + 1, z, n)
  | a == 0 = (p, z + 1, n)
  | a < 0  = (p, z, n + 1) 

closest :: Ord a => Num a => a -> [a] -> a 
closest _ [] = error "Empty list invalid input"
closest _ (x:[]) = x 
closest n (x : y : ys) 
  | n < x = x 
  | n > x && n < y && ((n-x) < (n-y)) = x
  | otherwise = closest n (y:ys)
  
data TernaryTree = Internal String TernaryTree TernaryTree TernaryTree |
                   Leaf String 

countNodes :: TernaryTree -> (Int, Int) 
countNodes tree = (countInternal tree, countLeaf tree) 

countInternal :: TernaryTree -> Int
countInternal (Leaf _) = 0 
countInternal (Internal _ i1 i2 i3) = 1 + countInternal i1 + countInternal i2 + countInternal i3 

countLeaf :: TernaryTree -> Int
countLeaf (Leaf _) = 1
countLeaf (Internal _ i1 i2 i3) = (countLeaf i1) + (countLeaf i2) + (countLeaf i3) 

random_list :: Int -> [Int] 
random_list seed = current : random_list next_seed
  where 
    next_seed = 7 * seed `mod` 101
    current = (next_seed - 1) `mod` 10 + 1 