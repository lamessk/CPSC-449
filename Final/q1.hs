recFL :: Show a => [a] -> String
recFL (x:[])   = show(x) 
recFL (x:y:[]) = show(x) ++ " and " ++ show(y)
recFL (x:xs) = show(x) ++ ", " ++ recFL xs

nrecFL :: Show a => [a] -> String 
nrecFL (x:[])   = show(x) 
nrecFL (x:y:[]) = show(x) ++ " and " ++ show(y)
nrecFL list = init(init((foldl nrecHelp "" (init list)))) ++ " and " ++ show(last list)


nrecHelp :: Show a => String -> a -> String
nrecHelp str x = str ++ show(x) ++ ", " 