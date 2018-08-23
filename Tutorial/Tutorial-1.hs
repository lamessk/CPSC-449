--XOR using pattern matching
exclusive :: Bool -> Bool -> Bool 
exclusive True False = True 
exclusive False True = True 
exclusive True True  = False
exclusive False False = False

--XOR using guards
ex2 :: Bool -> Bool -> Bool
ex2 x y 
    | (x || y) && (not (x && y)) = True
    | otherwise                  = False 

--Simplest way of implementing this function 
xor3 :: Bool -> Bool -> Bool 
xor3 x y = ((x || y) && (not (x && y)))


--show takes what is given and give a string representation of it 
xor4 :: Bool -> Bool -> String
xor4 x y 
    | (x || y) && (not (x && y)) = show (True)
    | otherwise                  = show (False)

-- if (i < 0) 
--    if (i < -5) 
--       cout << "A" << endl;
--    else 
--       cout << "B" << end1; 
ifex :: Int -> String 
ifex i
    | (i <= -5)              = "A"
    | (i < 0)                = ""
    | otherwise              = "B"

-- if True return max of x and y 
-- if False return min of x and y
maxMin :: Bool -> Int -> Int -> Int
maxMin True x y 
    | x > y     = x
    | otherwise = y
maxMin False x y 
    | x < y     = x
    | otherwise = y

--Recursive Functions 
-- compute factorial of a number
factorial :: Integer -> Integer
factorial n
    | n == 0   = 1
    | n > 0    = n*factorial(n-1)

-- find if a number x is a multiple of y 
-- if we wanted to retain the variable y we would make this a helper function 
perfectMul :: Int -> Int -> String 
perfectMul 0 _    = "First number is perfect multiple of the second" 
perfectMul x y 
    | (x < y)     = "First number is NOT a perfect multiple of the second" 
    | otherwise   = perfectMul (x - y) y 

--perfectMul2 :: Int -> Int -> String
--perfectMul2 x y 
 --   | (str == str2) = show (x) + " is a perfect multiple of " ++ show (y)
   -- | otherwise = show (x) + " Is not a perfect multiple of " ++ show (y) 
    --where 
      --str1 = "The first number is a perfect multiple of the second"
      --str2 = (perfectMul x y)

-- a student only completes half of his remaining work for an assignment in a single day
-- when the remaining words is less than 200, the student completes the rest of the assignment 
-- create a function that caculates how many days it would take the student to complete 
-- his assignment when how many words the assignment will contain 
daysToComp :: Double -> Int 
daysToComp 0 = 0
daysToComp x 
    | (x < 200) = 1
    | otherwise = 1 + (daysToComp(x / 2))

--same as the above, but with int as the param type
daysToCompI :: Int -> Int 
daysToCompI 0 = 0
daysToCompI x 
    | (x < 200) = 1
    | otherwise = 1 + (daysToCompI( ceiling((fromIntegral x) / 2)))

-- floor, ceiling, round are good to know for midterm and final 

--Calculate a power of a number 
powerNaive :: Double -> Int -> Double 
powerNaive _ 0 = 1 --Base case
powerNaive x y = x * (power x (y-1)) --A lot of recursive calls here. Crash when runs out of stack space 

--Non-naive 
power :: Double -> Int -> Double 
power _ 0 = 1
power x y 
    | y `mod` 2 == 1 = x * power x (y-1) 
    | otherwise = power (x * x)(y`div` 2) 	

-- Calculate sin to the n radians of precision. sin x = sum of (-1^k) * x^(2k+1) / (factorial 2k+1) for all values 0 <= k <= n 
sin2 :: Double -> Int -> Double 
sin2 x 0 = 0  
sin2 x n = (((power (-1) n) * (power x (2* (n + 1)))) / (fromIntegral (factorial (2*(n+1))))) + (sin2 x (n-1))






