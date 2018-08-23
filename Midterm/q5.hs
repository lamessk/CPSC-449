
countInteresting :: [a] -> [a] -> [Int]
countInteresting [] [] = []
countInteresting interesting pile = [(length (filter ( = first) pile))] ++ (countInteresting last1 pile)
  where 
    first = head intersting
	last1 = tail interesting

countInteresting :: Eq a => [a] -> [a] -> [Int] 
countInteresting [] _ = [] 
countInteresting (i:is) pile = length (filter (== i) pile) : countInteresting is pile 