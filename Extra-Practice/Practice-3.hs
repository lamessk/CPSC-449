import Data.Char

-- 5.2
maxThree :: Integer -> Integer -> Integer -> Integer
maxThree x y z 
    | x >= y && x >= z  = x
    | y >= z            = y
    | otherwise         = z

minThree :: Integer -> Integer -> Integer -> Integer
minThree x y z 
    | x <= y && x <= z = x
    | y <= z           = y
    | otherwise        = z

orderTriple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
orderTriple (a, b, c) 
  | (a /= max1) && (a /= min1) = (min1, a, max1)
  | (b /= max1) && (b /= min1) = (min1, b, max1)
  | (c /= max1) && (c /= min1) = (min1, c, max1)
  where 
    max1 = maxThree a b c 
    min1 = minThree a b c

-- 5.18 
doubleAll :: [Integer] -> [Integer] 
doubleAll values = [ 2*n | n <- values ]

-- 5.19 
capitalize :: String -> String
capitalize values = [ chr(ord(n) - 32) | n <- values]