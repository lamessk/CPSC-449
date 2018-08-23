nFib :: Int -> Int  
nFib 0 = 0
nFib 1 = 1 
nFib n = nFib (n-1) + nFib (n-2) 


-- running choice 10 (nFib 29) (nFib 30) (nFib 31) is a lot fatser than running this without choice (just running nFib)
-- running choice 0 takes longer than running choice 10 
-- 
choice :: Int -> a -> a -> a -> a 
choice n x y z
  | n > 0 = x
  | n == 0 = y
  | otherwise = z

-- Running multiply nFib 30 0 takes longer than multiply 0 nFib 30 
multiply :: Int -> Int -> Int 
multiply 0 _ = 0 
multiply _ 0 = 0 
multiply x y = x * y 

primes :: Int -> [Int] 
primes n = sieve [2 .. n]

--First number coming in has to be prime 
-- base case: []
-- Now process the rest of the list and get rid of all the multiples 
-- Take each value y out of the list and put a filtering contraint on it saying 
--that dividing by x cannot have a remainder of 0 (cannot be a multiple of x)
sieve :: [Int] -> [Int] 
sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x /= 0 ]
sieve [] = []


-- [2 ..] gives an infinite list 
-- take [2 ..] will return the first 10 elements of the infinite list 
-- Haskell is lazy so it'll only evaluate into the list as fast as it has to 
-- Sieve will do the same, only evaluate as far as necassary. 

--When calling this function, to get first 100 primes use "take 100 primesInf"
--Can get the 10,000th prime using primes !! 10000 (only evaluates up to the 10,000th and evaluates nothing more)
primesInf :: [Int] 
primesInf = sieve [2 ..]