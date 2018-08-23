import Data.Char 

maxOccurs :: Int -> Int -> (Int, Int) 
maxOccurs x y 
  | (x == y) && (max x y == x) = (x, 2)
  | (x == y) && (max x y == y) = (x, 2)
  | otherwise                  = (max x y, 1)

doubleAll :: [Int] -> [Int]
doubleAll values = [ n * 2 | n <- values ]

capitalize :: String -> String
capitalize values = [ toUpper n | n <- values, n >= 'a' && n <= 'z' ]

divisors :: Int -> [Int]
divisors n = divHelp n n 

divHelp :: Int -> Int -> [Int] 
divHelp n i
  | i == 0 = []
  | n `rem` i == 0 = i : divHelp n (i - 1)
  | otherwise      = divHelp n (i - 1)
  
isPrime :: Int -> Bool 
isPrime n 
  | n <= 0                                                = error "negative number"
  | head (divisors n) == n && head(tail(divisors n)) == 1 = True 
  | otherwise                                             = False 

matches :: Int -> [Int] -> [Int] 
matches x values = [ n | n <- values, n == x ]

elm :: Int -> [Int] -> Bool
elm x values 
  | length list == 0 = False 
  | otherwise        = True 
  where 
    list = matches x values  

onSeperateLines :: [String] -> String
onSeperateLines [] = "" 
onSeperateLines (x:xs) = x ++ "\n" ++ onSeperateLines xs

duplicate :: String -> Int -> String
duplicate _ 0 = ""
duplicate x n = x ++ duplicate x (n-1)

doubleAllr :: [Int] -> [Int] 
doubleAllr [] = []
doubleAllr (x:xs) = 2*x : doubleAllr xs 

doubleAllLC :: [Int] -> [Int] 
doubleAllLC values = [ n*2 | n <- values ] 

doubleAllMap :: [Int] -> [Int] 
doubleAllMap values = map (*2) values 

minlist :: [Int] -> Int 
minlist values = foldl1 (min) values 

pzn :: [Int] -> (Int, Int, Int)
pzn [] = (0, 0, 0) 
pzn values = foldl pznHelp (0, 0, 0) values 

pznHelp :: (Int, Int, Int) -> Int -> (Int, Int, Int) 
pznHelp (x, y, z) n 
  | n > 0 = (x + 1, y, z)
  | n ==0 = (x, y + 1, z)
  | n < 0 = (x, y, z + 1)  




















