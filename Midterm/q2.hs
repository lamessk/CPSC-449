cardScore :: String -> Int 
cardScore cards 
  | rank == "T" || rank == "K" = 10 
  | rank == "5"                = 5 
  | cards == "Dr"              = 25 
  | cards == "Ph"              = (-25) 
  | otherwise                  = 0   
  where 
    rank = tail cards 

listScore :: [String] -> Int 
listScore [] = 0
listScore (x:xs) = (cardScore x) + (listScore xs)