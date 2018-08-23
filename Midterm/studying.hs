import Data.Char
import Data.List 

comp :: Int -> Int 
comp x 
  | x < 0  = -1
  | x == 0 =  0
  | x > 0  =  1  

comp2 :: Int -> Int 
comp2 0 = 0
comp2 x 
  | x > 0 =  1
  | x < 0 = -1 
  
comp3 :: Int -> Int 
comp3 x 
  | x > 0 =  1
  | x < 0 = -1
comp3 _    = 0

ucase :: Char -> Char 
ucase ch 
  | ch < 'a' = ch
  | ch > 'z' = ch
  | otherwise = chr ((ord ch) - 32)
  
dChar :: Char -> String 
dChar ch = show(ch) ++ " is" ++ 
          (if (ch >= 'A' && ch <= 'Z') then "" else " not ") ++
           " an uppercase letter"  


isaVowel :: Char -> String 
isaVowel ch = case (ucase ch) of 
              'A' -> "Yes"
              'E' -> "Yes"
              'I' -> "Yes"
              'O' -> "Yes"
              'U' -> "Yes"
              'Y' -> "Sometimes" 
              _   -> "No" 



nfactorial :: Int -> Int 
nfactorial 0 = 1
nfactorial n = n * nfactorial(n-1)

mygcd :: Int -> Int -> Int 
mygcd x y 
  | x `rem` y == 0 = y 
  | otherwise      = mygcd y (x `rem` y) 


fibn :: Int -> Int 
fibn 0 = 0 
fibn 1 = 1 
fibn n = fibn(n-1) + fibn(n-2)


fib2 :: Int -> Int 
fib2 0 = 0 
fib2 1 = 1
fib2 n = fibH n 2 1 0 

fibH :: Int -> Int -> Int -> Int -> Int 
fibH n i fm1 fm2 
  | i < n = fibH n (i+1) (fm1 + fm2) fm1 
  | otherwise = fm1 + fm2 

isEven :: Int -> Bool 
isEven 0 = True 
isEven n = isOdd (n - 1)

isOdd :: Int -> Bool 
isOdd 0 = False
isOdd n = isEven (n - 1) 

-- Compute the length of a vector 
lenV :: (Double, Double, Double) -> Double 
lenV (x, y, z) = sqrt(x^2 + y^2 + z^2)

-- scale vector so it's length increases by a factor 
scale :: (Double, Double, Double) -> Double -> (Double, Double, Double) 
scale (x, y, z) n = (x*n, y*n, z*n) 

-- Normalize a vector so length = 1
normalize :: (Double, Double, Double) -> (Double, Double, Double) 
normalize values = scale values (1 / lenV values )

-- Compute dot product for 2 vectors 
dotProduct :: (Double, Double, Double) -> (Double, Double, Double)  -> Double 
dotProduct (x, y, z) (a, b, c) = (x * a) + (y * b) + (z * c) 

-- Compute cross product for 2 vectors 
crossProduct :: (Double, Double, Double) -> (Double, Double, Double)  -> (Double, Double, Double)
crossProduct (x, y, z) (a, b, c) = ((y*c - z*b), (z*a - x*c), (x*b - y*a))

isEmptyList :: [Int] -> Bool 
isEmptyList values 
  | length values == 0 = True 
  | otherwise          = False 

isSingleTonList :: [Int] -> Bool 
isSingleTonList values 
  | length values == 1 = True 
  | otherwise          = False 

isSingletonPM  :: [Int] -> Bool
isSingletonPM [x] = True 
isSingletonPM _ = False 

sumList :: [Int] -> Int 
sumList [] = 0
sumList values = head values + sumList ( tail values) 

mySum :: [Int] -> Int 
mySum [] = 0 
mySum (x:xs) = x + mySum(xs) 

decode :: [(Char, Int)] -> String 
decode []     = "" 
decode (x:xs) = replicate (snd x) (fst x) ++ decode xs

encode :: String -> [(Char, Int)] 
encode "" = [] 
encode s = (head s, prefixCount s) : encode ( drop (prefixCount s) s)

prefixCount :: String -> Int 
prefixCount [] = 0
prefixCount (x:[]) = 1
prefixCount (x1:x2: xs) 
  | x1 /= x2  = 1 
  | otherwise = 1 + prefixCount (x2:xs)

  
removeOdd :: [Int] -> [Int] 
removeOdd values = [ x | x <- values, x `rem` 2 == 0] 

ctof :: [Float] -> [Float] 
ctof values = [ n*(1.8) + 32 | n <- values ] 

squareEven :: [Int] -> [Int]
squareEven values = [ n*n | n <- values, n `rem` 2 == 0 ] 

titles :: [String] 
titles = ["The Matrix", 
          "Much Ado about nothing", 
          "The Princess Bride", 
          "An Accountant's nightmare",
          "The Adventures of Huckleberry Finn", 
          "Pride and Predjuice",
          "Alice's adventures in wonderland"]

-- To keep prefix, we will create a list of the titles with dropped Article 
-- The zip that list with original list of titles 
-- Then sort based on titles without the article (1st list in the tuple) 
-- Then unzip the tuples into 2 lists and take the second one (the one with articles) 
titleSort :: [String] -> [String] 
titleSort titles = snd(unzip (sort (zip [ dropArticle t | t <- titles] titles )))


-- Get a string in as a paramter 
-- Perform "words" to make string into a list of words
-- Take head of list (first letter) and check it is it "an, a, the" 
-- If it is any of those, return the title without the prefix
--Otherwise, return the title. 
dropArticle :: String -> String 
dropArticle s = case head (words s) of 
  "The" -> no_article_title
  "An"  -> no_article_title
  "A"   -> no_article_title
  otherwise -> s 
  where 
    no_article_title = unwords (tail (words s))

-- A function that computes a new list by applying a function 
-- to each element of the list 
myMap :: (Int -> Int) -> [Int] -> [Int] 
myMap _ [] = []
myMap f (x:xs) = f (x) : myMap f (xs) 

square :: Int -> Int 
square n = n*n 

-- Filter a list based on a Boolean function 
-- Goes over each element of list, if function returns true for 
-- the element, keep it, otherwise, discard it and go over the next
-- element of the list 
myFilter :: (Int -> Bool) -> [Int] -> [Int] 
myFilter _ [] = [] 
myFilter f (x:xs) 
  | f x == True = x : myFilter f xs 
  | otherwise   = myFilter f xs 

myMap2 :: (a -> b) -> [a] -> [b] 
myMap2 _ [] = [] 
myMap2 f (x:xs) = f x : myMap2 f xs 

myFilter2 :: (a -> Bool) -> [a] -> [a] 
myFilter2 _ [] = [] 
myFilter2 f (x:xs) 
  | f x == True = x : myFilter2 f xs
  | otherwise   = myFilter2 f xs 
  

plus :: Int -> Int -> Int 
plus x y = x + y 

foldSum :: [Int] -> Int
foldSum xs = foldl1 plus xs

foldMin :: [Int] -> Int 
foldMin xs = foldr1 min xs 

-- Use a fold to remove adjacent duplicates from a list 
removeAdjDup :: [Int] -> [Int] 
removeAdjDup xs = foldr removeHelper [] xs

removeHelper :: Int -> [Int] -> [Int] 
removeHelper left [] = [left]
removeHelper left (r:rs) 
  | left == r = (r:rs)
  | otherwise = (left:r:rs)

decRun :: [(Int, Char)] -> String 
decRun values = foldl decHelper "" values 

decHelper :: String -> (Int, Char) -> String
decHelper left (num, ch) = left ++ replicate num ch 

-- Sort a list using a fold (insertion sort algorithm) 
ins :: Int -> [Int] -> [Int]
ins x [] = [x] 
--This instruction tells us what the head of the list is 
ins x (y:ys) 
  | x < y = (x : y : ys) 
  --Recursive insert, how far to go into the list before inserting? 
  | otherwise = y : (ins x ys)  

foldSort :: [Int] -> [Int] 
foldSort values = foldr ins [] values 

zipping :: [Int] -> [Int] -> [Int]
zipping xs ys = zipWith plus xs ys 

multiply :: Int -> Int -> Int 
multiply x y = x * y 

foldDot :: [Int] -> [Int] -> Int 
foldDot xs ys = foldl1 plus (zipWith multiply xs ys)

sortString :: [String] -> [String] 
sortString values = sortBy sortHelper values 

sortHelper :: String -> String -> Ordering 
sortHelper s1 s2 
  | [toUpper ch | ch <- s1] == [toUpper ch | ch <- s2] = EQ
  | [toUpper ch | ch <- s1] <  [toUpper ch | ch <- s2] = LT 
  | otherwise = GT 

talk :: String -> String -> String
talk level message = map (volume level) message

volume :: String -> (Char -> Char)
volume "w" = toLower
volume "normal" = \ch -> ch 
volume "loud" = toUpper 

-- map each letter to ASCII
-- +n to each element of the string, modulo if it goes over the edge
-- map characters back to char 
caesar :: Int -> String -> String 
caesar n message = map chr (map (`mod` 128) (map (+ n) (map ord message)))

removefl :: [a] -> [a] 
removefl values
  | length values <= 2 = [] 
  | otherwise =  reverse (tail (reverse (tail values)))
  
removeflC :: [a] -> [a] 
removeflC values
  | length values <= 2 = []
  | otherwise = (reverse . tail . reverse . tail) values 

caesar2 :: Int -> String -> String 
caesar2 n message = map (chr . (`mod` 128) . (+ n) . ord) message  

class HasLength a where 
len :: a -> Int 

instance HasLength [a] where 
len x = length x 

mid3' :: Ord a => Num a => a -> a -> a -> a
mid3' x y z = x + y + z - (min (min x y) z) - (max (max x y) z)


