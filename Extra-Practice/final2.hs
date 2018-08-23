getSequence :: Int -> [Int] 
getSequence n = getSeqHelp [n] 

getSeqHelp :: [Int] -> [Int]  
getSeqHelp seq 
  | n > 1 && (n `rem` 2 == 0) = getSeqHelp (seq ++ [even]) 
  | n > 1 && (n `rem` 2 /= 0) = getSeqHelp (seq ++ [odd])
  | otherwise                 = seq
  where 
    n = last seq   
    even = n `div` 2 
    odd = 3 * n + 1

findSequences :: Int -> Int -> [Int]
findSequences n l = fSeqHelp n l 1 []

fSeqHelp :: Int -> Int -> Int -> [Int] -> [Int] 
fSeqHelp n l i seq
  | length seq             == n = seq   
  | length (getSequence i) == l = fSeqHelp n l (i+1) (seq ++ [i])
  | otherwise                   = fSeqHelp n l (i+1) seq 
  
getPreyLists :: [(String, String)] -> [(String, [String])] 
getPreyLists ppList = foldl ppHelp [] ppList

ppHelp :: [(String, [String])] -> (String, String) -> [(String, [String])]
ppHelp [] (npred, nprey) = [(npred, [nprey])]
ppHelp ((pred, preyList):rest) (npred, nprey)
  | pred == npred = (pred, preyList ++ [nprey]) : rest  
  | otherwise     = (pred, preyList) : ppHelp rest (npred, nprey)
  
data Eon = Hadean | Archean | Proterozoic | Phanerozoic deriving (Eq, Ord, Enum) 

instance Show Eon where
  show Hadean = "Hadean (4.6 billion to 4.0 billion years ago)" 
  show Archean = "Archean (4.0 billion to 2.5 billion years ago)"
  show Proterozoic = "Proterozoic (2.5 billion to 541 million years ago)"
  show Phanerozoic =  "Phanerozoic (541 million years ago to present)"
  
yearToEon :: Int -> Eon 
yearToEon year 
  | year >  2017 || year < -4600000000 = error "Year must be within range -4600000000"
  | year >= -4600000000 && year < -4000000000 = Hadean
  | year >= -4000000000 && year < -2500000000 = Archean 
  | year >= -2500000000 && year < -541000000  = Proterozoic
  | year >= -541000000  && year <= 2017       = Phanerozoic