data RPS = Rock | Paper | Scissors deriving Eq
 
instance Show RPS where 
  show Rock = "Used Rock" 
  show Paper = "Used Paper" 
  show Scissors = "Used Scissors"

data Outcome = Win | Tie | Lose deriving Eq 

game :: RPS -> RPS -> Outcome 
game Rock Scissors = Win 
game Scissors Paper = Win 
game Paper Rock =Win 
game Rock Rock = Tie 
game Paper Paper = Tie 
game Scissors Scissors = Tie 
game x y = Lose 

cToRPS :: Char -> RPS 
cToRPS 'r' = Rock 
cToRPS 'p' = Paper 
cToRPS 's' = Scissors
cToRPS x = error (show x ++ "is not a r, p, or s")

compete :: String -> String -> (Int, Int) 
compete you them = foldr competeHelp (0,0) (zip (map cToRPS you) (map cToRPS them)) 

competeHelp :: (RPS, RPS) -> (Int, Int) -> (Int, Int)
competeHelp (x,y) (w, l) 
  | game x y == Tie = (w, l)
  | game x y == Win = (w + 1, l) 
  | game x y == Lose = (w, l + 1) 
  
treeList1 :: [[Int]] 
treeList1 = [[1,1,2,2], [1,1,2,2]]

treeList2 :: [[Int]]
treeList2 = [[1,1,2,2,3,3,4,4], [1,1,2,2,3,3,4,4], [1,1,2,2,3,3,4,4]]

data Tree = Node Tree Tree | Leaf Int Int Int 

createTree :: [[Int]] -> Tree 
createTree [] = error "list empty" 
createTree xs 
  | isUniform xs = Leaf (head hlist) len (length xs) 
  | otherwise = Node (createTree (map (take hlfL) xs)) (createTree (map (drop hlfL) xs))
  where 
    hlist = head xs
    len = length hlist 
    hlfL = round ((fromIntegral len)/2)

isUniform :: [[Int]] -> Bool 
isUniform xs = foldr (&&) True (map rowUniform xs)

rowUniform :: [Int] -> Bool 
rowUniform [] = True 
rowUniform (x:[]) = True 
rowUniform (x:y:xs)
  | x == y = rowUniform (y:xs) 
  | otherwise = False 


instance Show Tree where 
  show (Leaf num w h) = "Leaf of " ++ (show num) ++ ", width " ++ (show w) ++ ", height" ++ (show h) 
  show (Node t1 t2)   = " A Node \n" ++ (show t1) ++ (show t2)
  
  
treeToList :: Tree -> [[Int]] 
treeToList (Leaf n w h) = genLeaf n w h 
treeToList (Node t1 t2) = zipWith (++) (treeToList t1) (treeToList t2) 

genLeaf :: Int -> Int -> [[Int]] 
genLeaf n w 0 = [] 
genLeaf n w h = (([ n | k <- [1 .. w] ]) : (genLeaf n w (h-1)))




















