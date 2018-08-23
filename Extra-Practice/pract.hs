onlyIncreasingLeft ::[Int] -> [Int]
onlyIncreasingLeft vals = foldl leftHelper [] vals 

leftHelper :: [Int] -> Int -> [Int]
leftHelper [] x = [x]  
leftHelper values x
  | x < last values  = values 
  | otherwise        = values ++ [x] 

myButLast :: [a] -> a
myButLast [] = error "empty list"
myButLast values = head tl 
 where 
   rev = reverse values
   tl = tail rev 
   