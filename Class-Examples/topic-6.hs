import Data.Char

-- Need to do 4 things to each character: Convert to ASCII using ord, add shift, modulo if it goes over the edge, covert back to char 
-- Use map 4 times to do these 4 things
-- +amt fixes right operand to amount using partial application 

caesar :: Int -> String -> String
caesar amt message = map chr (map (`mod` 128) (map (+ amt) (map ord message)))


--Polymorhpic so it works on any type of list 
-- commented out version works but its not great
-- Want to compose a function such that we don't have to keep creating so
-- many intermediate lists like the first way did
removeFL :: [a] -> [a] 
removeFL xs 
  | length xs <= 1 = [] 
--  | otherwise      = (reverse (tail (reverse (tail xs))))
  | otherwise = (reverse . tail . reverse . tail) xs

-- Only map over the list once now with function composition. Same result but not so many intermediate lists 
caesarf :: Int -> String -> String
caesarf amt message = map ( chr . (`mod` 128) . (+ amt) . ord) message 

mid3 :: Ord a => a -> a -> a -> a
mid3 x y z
  | x <= y && y <= z = y
  | z <= y && y <= x = y
  | y <= z && z <= x = z
  | x <= z && z <= y = z
  | y <= x && x <= z = x
  | z <= x && x <= y = x

-- This find the middle of 3 numbers by finding the max of 3, the min of 3, subtracting those values to the sum 
-- so that the middle of three remains after the subtraction 
-- Assumes a is some numeric type when we just have a-> a -> a -> a so we add Num => 
-- We also add Ord => a because there needs to be some sort of ordering as well
mid3' :: Ord a => Num a => a -> a -> a -> a
mid3' x y z = x + y + z - (min (min x y) z) - (max (max x y) z)