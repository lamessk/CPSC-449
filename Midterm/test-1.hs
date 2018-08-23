percentLookup :: [a] -> Float -> a 
percentLookup [] _ = error "percentLookup failed due to an empty list"
percentLookup values percent 
  | percent <= 0   = head values 
  | percent >= 100 = last values 
  | otherwise      = values !! (floor ((percent/100) * (fromIntegral(length values))))

windci :: Float -> Float -> Float 
windci temp speed 
  | temp > 10 || speed <= 4.8 = temp 
  | otherwise = 13.12 + 0.6215 * temp - 11.37 * (speed** 0.16) + 0.3965 * temp * (speed**0.16)

windChills :: [(Float, Float)] -> [Float]
windChills values = [ windci x y | (x, y) <- values ] 


filterLimit :: (a -> Bool) -> Int -> [a] -> [a] 
filterLimit _ _ [] = [] 
filterLimit f limit (x:xs) 
  | limit == 0 = (x:xs)
  | (f x == False) =  filterLimit f (limit - 1) xs
  | otherwise      = filterLimit f limit xs 

pathLength :: [(Float, Float)] -> Float
pathLength [] = 0
pathLength values = snd (foldl plHelper (head values, 0) (tail values))

plHelper :: ((Float, Float), Float) -> (Float, Float) -> ((Float, Float), Float)
plHelper ((x1, y1), dist) (x2, y2) = ((x2, y2), dist + sqrt (((x1 - x2) ** 2) + ((y1 - y2) ** 2)))

