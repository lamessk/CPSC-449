-- A
-- D
-- E
-- C
-- A
-- C
-- A
-- D
-- D
-- D 
-- redact s = map (\x | x /= ' ' = *) s 
-- map sqrt (map (abs values))

toDogYears :: Float -> Float 
toDogYears n 
  | n == 0 = n
  | n < 0 = negativeHelper n
  |(n > 0) && (n < 1) = (10.5*n) + toDogYears (n - n) 
  | n > 2 = 4.0 + toDogYears(n - 1)
  | otherwise = 10.5 + toDogYears (n - 1)    

negativeHelper :: Float -> Float
negativeHelper n 
  | n == 0 = n
  |(n < 0) && (n > (-1)) = ((-10.5)*n) + toDogYears (n + n) 
  | n < (-2.0) = (-4.0) + negativeHelper (n + 1) 
  | otherwise = (-10.5) + negativeHelper (n + 1) 

recEM :: [Int] -> [Int]
recEM [] = [] 
recEM (x:xs) 
  | (x `rem` 3 == 0) = recEM xs 
  | (x `rem` 5 == 0) = recEM xs
  | otherwise        = x : recEM xs 
  
lcEM :: [Int] -> [Int] 
lcEM values = [ n | n <- values, (n `rem` 3 /= 0), (n `rem` 5 /= 0) ]

higherEM :: [Int] -> [Int] 
higherEM values = filter (\x -> x `rem` 5 /= 0) (filter (\x -> x `rem` 3 /= 0) values)

onlyIncreasingLeft :: [Int] -> [Int] 
onlyIncreasingLeft vals = foldl leftHelper [] vals 

leftHelper :: [Int] -> Int -> [Int] 
leftHelper [] val = [val]
leftHelper nums val  
  | val <= lastElm = nums 
  | otherwise      = nums ++ [val]   
  where 
    lastElm = last nums


onlyIncreasingRight :: [Int] -> [Int] 
onlyIncreasingRight vals = foldr rightHelper [] vals 

rightHelper :: Int -> [Int] -> [Int] 
rightHelper val [] = [val]
rightHelper val nums  
  | val >= lastElm = replace head nums val  
  | otherwise      = [val] ++ nums
  where 
    lastElm = head nums


dropUntil :: (a -> Bool) -> [a] -> [a] 
dropUntil _ [] = [] 
dropUntil f (x:xs) 
  | f x == False = dropUntil f xs
  | otherwise    = [x] ++ xs 
  























