import Data.List

titles :: [String] 
titles = ["The Matrix", 
          "Much Ado about nothing", 
          "The Princess Bride", 
          "An Accountant's nightmare",
          "The Adventures of Huckleberry Finn", 
          "Pride and Predjuice",
          "Alice's adventures in wonderland"]

titleSort :: [String] -> [String]
titleSort titles = snd (unzip (sort (zip [dropArticle t | t <- titles] titles))) 

--Return a new copy of the string with a leading A, An or The removed 
-- if one is prefernt. If not present, return original string
--One solution with pattern matching, beter solution with case 

dropArticle :: String -> String 
--dropArticle ('A': ' ': rest_of_title) = rest_of_title 
--dropArticle ('A': 'n': rest_of_title) = rest_of_title 
--dropArticle ('T': 'h' : 'e' : ' ': rest_of_title) = rest_of_title 
--dropArticle s = s
dropArticle s = case head (words s) of 
  "The" -> no_article_title
  "An"  -> no_article_title
  "A"   -> no_article_title
  otherwise -> s 
  where 
    no_article_title = unwords (tail (words s))



myMap :: (Int -> Int) -> [Int] -> [Int] 
myMap _ [] = [] 
myMap f (x:xs) = f x : myMap f xs 