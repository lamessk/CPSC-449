--tutorial 2
import Data.Char

--example of how a string and a list of chars are the same thing
--Running with "hello" and ['h', 'e', 'l', 'l', 'o'] produces same result
stringlen :: [Char] -> Int
stringlen [] = 0
stringlen (x:xs) = 1 + stringlen(xs)

-- Remove everything but vowels in a sentence 
-- Solved using pattern matching
-- If a character is matched it is inserted into the new list and the 
-- program recursively runs on the rest of the program 
-- Otherwise, the first character is none of the ones mentioned, and we
-- do not add anything to the list of vowels, just recursively call on the
-- remaining string 
onlyVowels :: [Char] -> [Char] 
onlyVowels [] = []             -- Base case, no characters, return nothing
onlyVowels ('a':rest) = ('a' : (onlyVowels rest))
onlyVowels ('e':rest) = ('e' : (onlyVowels rest))
onlyVowels ('i':rest) = ('i' : (onlyVowels rest))
onlyVowels ('o':rest) = ('o' : (onlyVowels rest))
onlyVowels ('u':rest) = ('u' : (onlyVowels rest))
onlyVowels ('y':rest) = ('y' : (onlyVowels rest))
onlyVowels ('A':rest) = ('A' : (onlyVowels rest))
onlyVowels ('E':rest) = ('E' : (onlyVowels rest))
onlyVowels ('I':rest) = ('I' : (onlyVowels rest))
onlyVowels ('O':rest) = ('O' : (onlyVowels rest))
onlyVowels ('U':rest) = ('U' : (onlyVowels rest))
onlyVowels ('Y':rest) = ('Y' : (onlyVowels rest))
onlyVowels (x:rest) = (onlyVowels rest)

-- Function that converts lower case letters to upper case letters
-- 'a' = 97, 'A' = 65, 'z' = 122
-- ord converts ascii to integer representation, chr converts int to char rep
-- Grab first character from list and use it for comparisons 
-- Do recursive call with the remaining part 
toCapl :: [Char] -> [Char]
toCapl [] = []
toCapl (x:xs)
  | ((num >= 97) && (num <= 122)) = ((chr (num - 32)) : (toCapl xs))  -- lower case converted
  | ((num >= 65) && (num <= 90)) = (x : (toCapl xs))  -- already capitalised 
  | otherwise = error (show(x) ++ "is not a letter \n") 
  where 
    num = ord x

-- Does same as toCapl with a list comprehsion 
toCapl2 :: [Char] -> [Char]
toCapl2 list = [ chr (ord(n) - 32) | n <- list, ((ord n) >= 97), ((ord n) <= 122)] 

-- Map : takes a function and applies it to every element of a list
mapCap :: [Char] -> [Char] 
mapCap list = map capHelp list 

-- Helps with mapCap. Takes single characters and coverts them to capitals 
-- if they are lowercase, or returns character if not a lowercase to begin with 
capHelp :: Char -> Char 
capHelp x 
  | ((num >= 97) && (num <= 122)) = (chr (num - 32))
  | otherwise = x 
  where 
     num = (ord x)


-- A function that filters out specific undesired characters from a list 
-- Filter takes a function and applies it to each element. Anything the filter 
-- returns true is left in the list, anything false is removed 
-- Here we first filter out undesired characters, then we map remaining ones to capitals
filterAndCap :: [Char] -> [Char] 
filterAndCap list = map capHelp (filter filterHelp list) 

-- Returns true if character is an upper or lowercase letter 
-- Returns false for all other characters
filterHelp :: Char -> Bool
filterHelp x
  | ((num >= 97) && (num <= 122)) = True
  | ((num >= 65) && (num <= 90 )) = True 
  | otherwise                     = False 
  where 
    num = (ord x) 


power1 :: (Int, Int) -> Int 
power1 (x, y) = x^y

power2 :: (Int, Int) -> Int 
power2 tuple = fst(tuple)^snd(tuple)

-- Reverse a string 
reverseStr :: [Char] -> [Char]
reverseStr [] = [] 
reverseStr (x:xs) = reverseStr (xs) ++ [x]

--folds takes a function with two paramters 
--example:
--  foldl <identity> [1, 2, 3] -> (((identity + 1) + 2) + 3)   --uses identity and first element of list
--  foldr <identity> [1, 2, 3] -> (1 + (2 + ( 3+ identity)))  --uses identity and last element of list

sum' :: [Int] -> Int
sum' xs = foldl (+) 0 xs 


--example find max value in the list given as tuples REALLY IMPORTANT FOR MIDTERM AND FINAL 
--here we make our identity = 0 cause itll want to compare the first integer to zero.
getMax :: [(Int, Int)] -> Int
getMax list = foldr maxHelp <identity> list  

maxHelp :: (Int, Int) -> Int -> Int
maxHelp (x, y) z = (x `max` (y `max` z))

-- foldl -->>> foldl <2 paramter function> <identity> <list> 













