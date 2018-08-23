-- first parameter wrapped into a tuple
-- Return a number giving length 
-- use each part of the tuple directly using (x, y, z)
len :: (Double, Double, Double) -> Double
len (x, y, z) = sqrt( x ^ 2 + y ^ 2 + z ^ 2)

-- Scale a vector so it's length increases by a factor f 
scale :: (Double, Double, Double) -> Double -> (Double, Double, Double)
scale (x, y, z) f = ( f * x, f * y, f * z)

--Normalize a vector so it's length is 1 
--This is really ugly 
--Use where to get rid of repeated calculation? or see normalize2 
normalize :: (Double, Double, Double) -> (Double, Double, Double)
normalize (x, y, z) = (x / (len (x, y, z)), y / (len (x, y, z)), z / (len (x, y, z))) 

-- Use the previously defined functions to make it easier and prettier 
normalize2 :: (Double, Double, Double) -> (Double, Double, Double)
normalize2 v = scale v (1 / len v) 

-- Dot product of 2 vectors 
dot :: (Double, Double, Double) -> (Double, Double, Double) -> Double
dot (x1, y1, z1) (x2, y2, z2) = x1 * x2 + y1*y2 + z1*z2

--Cross product 
cross :: (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double, Double)
cross (x1, y1, z1) (x2, y2, z2) = (y1*z2 - z1*y2, z1*x2 - x1*z2, x1*y2 -y1*x2)

-- Return true if list is empty, false otherwise
isEmptyList :: [Int] -> Bool 
isEmptyList values
    | length values == 0 = True 
    | otherwise          = False

-- Same as above but with pattern matching 
isEmptyListPM :: [Int] -> Bool 
isEmptyListPM [] = True 
isEmptyListPM _  = False

-- Does the list have a length of one? 
isSingletonList :: [Int] -> Bool 
isSingletonList values 
    | length values == 1 = True 
    | otherwise          = False 

-- Pattern Matching Singleton 
isSingletonListPM :: [Int] -> Bool
isSingletonListPM [_] = True 
isSingletonListPM _   = False 

-- Pattern Matching Singleton 
isSingletonListPM2 :: [Int] -> Bool
isSingletonListPM2 (_:[]) = True 
isSingletonListPM2 _   = False 

-- Sum all elements in a list
mySum :: [Int] -> Int 
mySum [] = 0
mySum values = head values + mySum (tail values)

-- Sum using pattern matching 
-- Take head of list and add it to mySum of tail of the list 
mySum2 :: [Int] -> Int 
mySum2 [] = 0 
mySum2 (v:vs) = v + mySum2 vs 

-- Could have used replicate function here OR could have created a recursive replaicate function that did it for us 
decode :: [(Char, Int)] -> String 
decode [] =""
decode (x:xs) = replicate (snd x) (fst x) ++ decode xs   -- break up the list by tail(xs) and head (x), call recursively on tail of the list 
--decode ((ch, n):xs) = replicate n ch ++ decode xs      -- Could use this to do the same as fst and snd but with pattern matching (pick one) 



-- Paramaters: A string of characters 
-- Returns the number of copies of the same character that appear at the beginning of the string 
prefixCount  :: String -> Int 
prefixCount [] = 0
prefixCount (x:[]) = 1 
prefixCount (x1:x2:xs)      -- compare two consecutive characters and check if they're the same.
    | x1 /= x2 = 1          -- if they are not, we are at the end of the same chars 
    | x1 == x2 = 1 + prefixCount (x2:xs)


encode :: String -> [(Char, Int)]
encode "" = []
encode s = (head s, prefixCount s) : (encode (drop (prefixCount s) s))

removeOdd :: [Int] -> [Int]
removeOdd values = [n | n  <- values, n `rem` 2 == 0]

c2f :: [Double] -> [Double]
c2f temps = [9/5 * t + 32 | t <- temps] 

squareEven :: [Int] -> [Int] 
squareEven values = [n * n | n <- values, n `rem` 2 == 0]




