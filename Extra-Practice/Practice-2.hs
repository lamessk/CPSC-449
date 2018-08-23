-- 4.1 

maxThree :: Int -> Int -> Int -> Int
maxThree x y z 
    | x >= y && x >= z  = x
    | y >= z            = y
    | otherwise         = z

maxFour :: Int -> Int -> Int -> Int -> Int
maxFour a b c d 
    | maxThree a b c > d = maxThree a b c
    | otherwise          = d

maxFourf :: Int -> Int -> Int -> Int -> Int
maxFourf a b c d 
    | max a b > max c d = max a b 
    | otherwise         = max c d

maxFourBoth :: Int -> Int -> Int -> Int -> Int
maxFourBoth a b c d = max (maxThree a b c) (d) 

-- 4.2 

between :: Integer -> Integer -> Integer -> Bool
between m n p 
    | weakAscendingOrder m n p = True 
    | otherwise                = False 

weakAscendingOrder :: Integer -> Integer -> Integer  -> Bool
weakAscendingOrder a b c 
    | (a <= b) && (b <= c) = True 
    | otherwise            = False

-- 4.3
twoEqual :: Int -> Int -> Bool
twoEqual a b = (a == b)

howManyEqual :: Int -> Int -> Int -> Int
howManyEqual a b c
    | (twoEqual a b) && (b == c)                         = 3
    | (twoEqual a b) || (twoEqual b c) || (twoEqual a c) = 2
    | otherwise                                          = 0

-- 4.4 
howManyOfFourEqual :: Int -> Int -> Int -> Int -> Int
howManyOfFourEqual a b c d
    | (howManyEqual a b c == 3) && (howManyEqual a b d == 3) && (howManyEqual b c d == 3)                              = 4 
    | (howManyEqual a b c == 3) || (howManyEqual a b d == 3) || (howManyEqual b c d == 3) || (howManyEqual a c d == 3) = 3
    | (howManyEqual a b c == 2) || (howManyEqual a b d == 2) || (howManyEqual b c d == 2) || (howManyEqual a c d == 2) = 2
    | otherwise                                                                                                        = 0

-- 4.9 
maxThreeOccurs :: Int -> Int -> Int -> (Int, Int)
maxThreeOccurs a b c
  | (a == b) && (b == c)                                                 = (m, n)
  | (m == a) && (a == b) || (m == b) && (b == c) || (m == c) && (a == c) = (m, n-1)
  | otherwise                                                            = (m, n-2)
  where 
    m = maxThree a b c
    n = 3

-- 4.17
rangeProduct :: Integer -> Integer -> Integer 
rangeProduct m n 
    | n < m  = 0
    | n == m = m 
    | n > m = n * rangeProduct m (n-1)

-- 4.18 
facRange :: Integer -> Integer
facRange a 
  | a == 0    = 1
  | otherwise = rangeProduct 1 a

-- 4.19 
recMult :: Int -> Int -> Int 
recMult a b 
   | a == 0 || b == 0 = 0
   | otherwise        = a + recMult a (b - 1)




