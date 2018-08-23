import Data.List 
import Data.Char


-- foldr <function to insert between each item in the list> 
-- <initial value to build initial fold> <list to process>
-- Has different return time 
removeAdjDup :: [Int] -> [Int]
removeAdjDup values = foldr removeHelper [] values

-- Will take last member of values as first param and initial value out beyond end of list as second param
removeHelper :: Int -> [Int] ->[Int]
--Add this case in pattern match in case of empty list 
removeHelper left [] = [left]
-- This says list alwasy have to have at least 1 element
removeHelper left (r:rs)
  | left == r = (r:rs)
  | otherwise = (left:r:rs) 

--Decode shown on slide 20 
decHelper :: String -> (Int, Char) -> String 
decHelper left (count, ch) = left ++ (replicate count ch)

decRun :: [(Int, Char)] -> String 
decRun values = foldl decHelper "" values 

-- Sort a list using a fold (insertion sort algorithm) 
ins :: Int -> [Int] -> [Int]
ins x [] = [x] 
--This struction tells us what the head of the list is 
ins x (y:ys) 
  | x < y = (x : y : ys) 
  --Recursive insert, how far to go into the list before inserting? 
  | otherwise = y : (ins x ys)  

foldSort :: [Int] -> [Int] 
foldSort values = foldr ins [] values 

--dotProduct :: [Int] -> [Int] -> Int 
--dotProduct v1 v2 = fold plus (zipWith multiply v1 v2) 

plus :: Int -> Int -> Int 
plus x y =  x + y 

multiply :: Int -> Int -> Int 
multiply x y = x * y

ciSort :: [String] -> [String]
ciSort values = sortBy insCompare values 

insCompare :: String -> String -> Ordering 
insCompare s1 s2
  | [toUpper ch | ch <- s1] == [toUpper ch | ch <- s2] = EQ
  | [toUpper ch | ch <- s1] <  [toUpper ch | ch <- s2] = LT 
  | otherwise = GT 

talk :: String -> String -> String 
talk level message = map (volume level) message 

-- Params : level of volume, 
-- Maping volume level over message, which is a string 
-- Needs to pattern matching against all three levels
-- To upper exists as part of data.char 
-- ToLower also exists in data.char 
-- For normal we write a lamda function that returns the input 
volume :: String -> (Char -> Char) 
volume "whisper" = toLower
volume "normal" = \ch -> ch 
volume "loud" = toUpper 



