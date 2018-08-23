import Data.Char

exclusive :: Bool -> Bool -> Bool 
exclusive True False = True 
exclusive False True = True 
exclusive _ _ = False

--nAnd :: Bool -> Bool -> Bool 
--nAnd True True = False
--nAnd _ _ = True

nAnd :: Bool -> Bool -> Bool 
nAnd a b = (not a) || (not b)

mystery :: Integer -> Integer -> Integer -> Bool 
mystery m n p = not ((m ==n) && (n==p))

threeDifferent :: Integer -> Integer -> Integer -> Bool 
threeDifferent a b c = not((a==b) || (b==c) || (a==c))

threeEqual :: Integer -> Integer -> Integer -> Bool
threeEqual m n p = (m==n) && (n==p)

fourWithThree :: Integer -> Integer -> Integer -> Integer -> Bool
fourWithThree m n p q = (threeEqual m n p) && (p==q)

fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual m n p q = (m==n) && (n==p) && (p==q)

myMax :: Integer -> Integer -> Integer 
myMax x y 
    | x >= y    = x
    | otherwise = y

maxThree :: Integer -> Integer -> Integer -> Integer
maxThree x y z 
    | x >= y && x >= z  = x
    | y >= z            = y
    | otherwise         = z

myMin :: Integer -> Integer -> Integer 
myMin x y 
    | x <= y    = x
    | otherwise = y

minThree :: Integer -> Integer -> Integer -> Integer
minThree x y z 
    | x <= y && x <= z = x
    | y <= z           = y
    | otherwise        = z


tocapital :: Char -> Char 
tocapital ch 
    | ch < 'a' = ch
    | ch > 'z' = ch
    | otherwise = (chr(ord(ch) - 32))

charToNum :: Char -> Int 
charToNum ch
    | ch < '0' = 0
    | ch > '9' = 0 
    | otherwise = digitToInt(ch) 


onThreeLines :: String -> String -> String -> String 
onThreeLines a b c = a ++ "\n" ++ b ++ "\n" ++ c ++ "\n"

romanDigit :: Char -> String 
romanDigit ch 
    | ch == '1' = "I"
    | ch == '2' = "II"
    | ch == '3' = "III"
    | ch == '4' = "IV"
    | ch == '5' = "V"
    | ch == '6' = "VI"
    | ch == '7' = "VII"
    | ch == '8' = "VIII"
    | ch == '9' = "IX"

averageThree :: Integer -> Integer -> Integer -> Float
averageThree a b c = fromIntegral(a+b+c) / 3  

howManyAboveAverage :: Integer -> Integer -> Integer -> Integer 
howManyAboveAverage a b c = (if fromInteger a > averageThree a b c then 1 else 0) + 
                            (if fromInteger b > averageThree a b c then 1 else 0) + 
                            (if fromInteger c > averageThree a b c then 1 else 0)


numberNDroots :: Float -> Float -> Float -> Integer 
numberNDroots a b c 
    | b*b - 4*a*c == 0 = 1
    | b*b - 4*a*c > 0 = 2
    | b*b - 4*a*c < 0 = 0






