collapseSpaces :: String -> String 
collapseSpaces message = foldl leftHelper "" message 

leftHelper :: String -> Char -> String 
leftHelper "" a = [a]   
leftHelper values ch 
  | ch == ' ' && (last values) == ' ' = values 
  | otherwise                         = values ++ [ch] 