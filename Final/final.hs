import Data.Char
import Data.List

truthTable ::Bool -> Bool -> Bool 
truthTable False True = True 
truthTable _ _ = False 

myDiv :: Float -> Float -> Float
myDiv _ 0 = error "cannot divide by 0" 
myDiv a b = a / b 

comp :: Int -> Int 
comp x 
  | x < 0 = -1
  | x == 0 = 0 
  | otherwise = 1 

comp2 :: Int -> Int 
comp2 0 = 0
comp2 x 
  | x > 0 = 1
  | x < 0 = -1

ucase :: Char -> Char 
ucase ch 
  | ch < 'a' = ch 
  | ch > 'z' = ch 
  | otherwise = chr((ord ch) - 32) 

describeChar :: Char -> String 
describeChar ch = show(ch) ++ "is " ++ 
                  (if (ch >= 'A') && (ch <= 'Z') then "is " else ("is not ")) ++
                  "an uppercase letter" 

factorial :: Int -> Int 
factorial 0 = 1
factorial n = n * factorial(n - 1)

mygcd :: Int -> Int -> Int 
mygcd x y 
  | (x `rem` y == 0) = y 
  | otherwise        = mygcd y (x `rem` y) 

nFib :: Int -> Int 
nFib 0 = 0
nFib 1 = 1
nFib n = nFib(n - 1) + nFib(n - 2) 

iFib :: Int -> Int 
iFib 0 = 0
iFib 1 = 1
iFib n = iFibH n 2 1 0 

iFibH :: Int -> Int -> Int -> Int -> Int 
iFibH n i fm1 fm2 
  | i < n = iFibH n (i+1) (fm1 + fm2) fm1 
  | otherwise = fm1 + fm2   

isEven :: Int -> Bool 
isEven 0 = True 
isEven n = isOdd(n - 1)

isOdd :: Int -> Bool 
isOdd 0 = False
isOdd n = isEven(n - 1) 

vLen :: (Double, Double, Double) -> Double 
vLen (x, y, z) = sqrt(x^2 + y^2 + z^2)

scale :: (Double, Double, Double) -> Double -> (Double, Double, Double) 
scale (x, y, z) a = (a * x, a * y, a * z)

normalize :: (Double, Double, Double) -> (Double, Double, Double) 
normalize (x, y, z) = (x / (vLen (x, y, z)), y / (vLen (x, y, z)), z / (vLen (x, y, z))) 

dot :: (Double, Double, Double) -> (Double, Double, Double) -> Double
dot (a, b, c) (x, y, z) = (a*x) + (b*y) + (c*z) 

cross :: (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double, Double)
cross (a, b, c) (x, y, z) = ((b*z - c* y), (c*x - a*z), (a*y - b*x))

isEmptyPM :: [Int] -> Bool 
isEmptyPM [] = True 
isEmptyPM _ = False 

isEmptyGuards :: [Int] -> Bool
isEmptyGuards x 
  | length x == 0 = True 
  | otherwise     = False 

-- Can also use length to find out if this is a singleton list with length == 1
isSingletonList :: [Int] -> Bool 
isSingletonList [_] = True 
isSingletonList _ = False 

mySum :: [Int] -> Int 
mySum [] = 0 
mySum (x:xs) = x + mySum(xs) 

decode :: [(Char, Int)] -> String 
decode [] = "" 
decode (x:xs) = replicate (snd x) (fst x) ++ decode(xs)

encode :: String -> [(Char, Int)] 
encode "" = [] 
encode s = (head s, (prefixCount s)) : (encode (drop (prefixCount s) s))

prefixCount :: String -> Int 
prefixCount "" = 0 
prefixCount (x:[]) = 1
prefixCount (x:y:xs) 
  | (x == y) = 1 + prefixCount (y:xs) 
  | otherwise = 1 
   
-- Remove all odd numbers
removeOdd :: [Int] -> [Int] 
removeOdd oldList = [ n | n <- oldList, n `rem` 2 == 0 ] 

--From celcius to fahrenheit
c2f :: [Double] -> [Double] 
c2f temps = [9/5 * t + 32 | t <- temps ] 

squareEven :: [Int] -> [Int]
squareEven ints = [ n^2 | n <- ints, n `rem` 2 == 0]

titles :: [String] 
titles = ["The Matrix", 
          "Much Ado about nothing", 
          "The Princess Bride", 
          "An Accountant's nightmare",
          "The Adventures of Huckleberry Finn", 
          "Pride and Predjuice",
          "Alice's adventures in wonderland"]

--Drop the article of each title 
--Zip version with article with version without article 
-- Sort according to the one without an article 
--Unzip and take the second of the two lists 
titleSort :: [String] -> [String] 
titleSort titles = snd(unzip(sort(zip [dropArticle t | t <- titles] titles)))

-- If title starts with an article, drop it. Return string without article. 
dropArticle :: String -> String 
dropArticle s = case head (words s) of
  "The" -> title_no_article 
  "An"  -> title_no_article
  "A"   -> title_no_article
  otherwise -> s
  where
    title_no_article =  unwords(tail(words s)) 

square :: Int -> Int 
square n = n*n

myMap :: (a -> b) -> [a] -> [b] 
myMap _ [] = [] 
myMap f (x:xs) = f x : myMap f xs 

myFilter :: (a -> Bool) -> [a] -> [a] 
myFilter _ [] = [] 
myFilter f (x:xs) 
  | f x == True = x : myFilter f xs 
  | otherwise   = myFilter f xs 

applyToTuple :: (a -> a) -> (a, a, a) -> (a, a, a) 
applyToTuple f (x,y,z) = (f x, f y, f z)

--Foldl1 
leftFold :: (a -> a -> a) -> [a] -> a 
leftFold _ [] = error "Left fold cannot be applied to an empty list" 
leftFold _ (x:[]) = x
leftFold f (x:xs) = f (leftFold f (init xs)) (last xs)

--Foldr1 
rightFold :: (a -> a -> a) -> [a] -> a 
rightFold _ [] = error "right fold cannot be applies to an empty list"
rightFold _ (x:[]) = x 
rightFold f (x:xs) = f x (rightFold f xs)

plus :: Int -> Int -> Int 
plus a b = a + b 

foldSum :: [Int] -> Int 
foldSum sums = foldl plus 0 sums

foldMin :: [Int] -> Int 
foldMin xs = foldr1 min xs 

foldReverse :: [Int] -> [Int] 
foldReverse xs = foldr reverseHelp [] xs 

reverseHelp :: Int -> [Int] -> [Int] 
reverseHelp a [] = [a] 
reverseHelp a xs = xs ++ [a] 

adjDup :: [Int] -> [Int] 
adjDup xs = foldr adjHelper [] xs 

adjHelper :: Int -> [Int] -> [Int] 
adjHelper left [] = [left] 
adjHelper left (r:rs)
  | left == r = (r:rs)
  | otherwise = (left:r:rs) 

decodeFold :: [(Int, Char)] -> String 
decodeFold list = foldl decHelp "" list 

decHelp :: String -> (Int, Char) -> String 
decHelp s (x, y) = s ++ (replicate x y) 

foldSort :: [Int] -> [Int] 
foldSort list = foldr ins [] list 

ins :: Int -> [Int] -> [Int] 
ins a [] = [a] 
ins a (x:xs) 
  | a < x = (a:x:xs) 
  | otherwise = x : (ins a xs)

multiply :: Int -> Int -> Int 
multiply a b = a * b 

zipDot :: [Int] -> [Int] -> Int 
zipDot list1 list2 = foldl plus 0 (zipWith multiply list1 list2) 

ciSort :: [String] -> [String]
ciSort values = sortBy insCompare values 

insCompare :: String -> String -> Ordering 
insCompare s1 s2
  | [toUpper ch | ch <- s1] == [toUpper ch | ch <- s2] = EQ
  | [toUpper ch | ch <- s1] <  [toUpper ch | ch <- s2] = LT 
  | otherwise = GT 

myFlip :: (a -> b -> c) -> (b -> a -> c)
myFlip f = \y x -> f x y 

talk :: String -> String -> String
talk level message = map (volume level) message 

volume :: String -> (Char -> Char) 
volume "loud" = toUpper
volume "normal" = (\ch -> ch) 
volume "whisper" = toLower

add5 :: [Int] -> [Int] 
add5 list = map (+ 5) list 

addn :: [Int] -> Int -> [Int] 
addn list n = map (+ n) list 

caesar :: String -> Int -> String 
caesar message n = map chr(map (`mod` 128) (map (+ n) (map ord message)))  

removeFL :: [a] -> [a] 
removeFL list 
  | length list <= 2 = error "list must be at least 3 characters long"
removeFL list =  (reverse . tail . reverse . tail) list 

caesar2 :: String -> Int -> String 
caesar2 message n = map (chr . (`mod` 128) . (+ n) . ord) message  

--This uses type classes so we can only allow types that cn be tested for equality 
allEqual :: Eq a => a -> a -> a -> Bool
allEqual x y z = (x == y) && (y == z)

-- This find the middle of 3 numbers by finding the max of 3, the min of 3, subtracting those values to the sum 
-- so that the middle of three remains after the subtraction 
-- Assumes a is some numeric type when we just have a-> a -> a -> a so we add Num => 
-- We also add Ord => a because there needs to be some sort of ordering as well
mid3 :: Ord a => Num a => a -> a -> a -> a
mid3 x y z = x + y + z - (min (min x y) z) - (max (max x y) z) 





