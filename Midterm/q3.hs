recEncrypt :: String -> String 
recEncrypt "" = ""
recEncrypt message = ([lastLetter] ++ restString) ++ " " ++  recEncrypt remaining
  where 
    mList = (words message)
    firstWord = head mList
    remaining =  unwords (tail mList) 
    lastLetter = last firstWord 
    restString = take (length(firstWord) - 1) firstWord

lcEncrypt :: String -> String 
lcEncrypt message = [ (last n) |  n <- mList ]
  where 
    mList = (words message)
    firstWord = head mList
    remaining =  unwords (tail mList) 
    lastLetter = last firstWord 
    restString = take (length(firstWord) - 1) firstWord
	
higherEncrypt :: String -> String 
higherEncrypt message = unwords (map 
  where 
    mList = words (message)

-- Solution for Part 1 
recEncrypt :: String -> String
recEncrypt s = unwords (recEncryptHelper (words s))

recEncryptHelper :: [String] -> [String] 
recEncryptHelper [] = [] 
recEncryptHelper (w:words) = (last w : init w) : recEncryptHelper words  

-- Solution for part 2 
lcEncrypt :: String -> String 
lcEncrypt s = unwords [last w : init w | w <- words s ] 

--Solution for part 3 
higherEncrypt :: String -> String
higherEncrypt s = unwords (map (\w -> last w : init w) (words s))
