
-- A type, Has length, that has type a -> Int 
class HasLength a where 
  len :: a -> Int 

-- An instance of types that have lengths is lists  (and strings)
instance HasLength [a] where 
  len x = length x 

-- Another instance is tuples of size 2 
-- Would have to also do this for every length of tuple 
instance HasLength (a, b) where 
  len _ = 2 

-- Forming a hierarchy with our own type classes
-- CanBeEmpty is a specialization of HasLength where length = 0 
-- CanBeEmpty would be below HasLength in the hierarachy 
class (HasLength a) => CanBeEmpty a where 
  isEmpty :: a -> Bool 
  
instance CanBeEmpty [a] where 
  isEmpty[] = True 
  isEmpty _ = False 

status :: CanBeEmpty a => a -> String 
status x 
  | isEmpty x = "Empty" 
  | otherwise = "contains " ++ show(len(x)) ++ " elements" 


data PrimaryColor = Red | Green | Blue deriving Show

--Uses default implementation of Eq
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving Eq

-- Creates our own implementation of Show 
-- (don't put deriving Show because it will think we mean the default implementation, not ours) 
instance Show Day where 
  show Monday = "Mon" 
  show Tuesday = "Tues" 
  show Wednesday = "Wed" 
  show Thursday = "Thurs" 
  show Friday = "Fri" 
  show Saturday = "Sat" 
  show Sunday = "Sun" 
  
isWeekend :: Day -> Bool 
isWeekend Sunday   = True
isWeekend Saturday = True
isWeekend _        = False

data RGBColor = RGB Int Int Int

data Shape = Circle Float |
             Rectangle Float Float | 
             Triangle Float Float Float deriving Show 

-- Remember to have brackets around the constructors 
area :: Shape -> Float 
area (Circle radius)  = 3.14 * radius 
area (Rectangle w h)  = w * h
area (Triangle a b c) = sqrt(p * (p - a) * (p - b) * (p - c))
  where 
    p = (a + b + c)/2

--Instance of Eq for shape data type. Tests if two shapes are equal.
--Two shapes are equal if their areas are within 0.001 of each other. 
--This allows us to say a 3x2 rect is the same as a 2x3 rect  
instance Eq Shape where 
  s1 == s2 = abs (area s1 - area s2) < 0.001 

-- Used to be just for ints, now polymorphic 
-- Added a beside definition and made all Int's a's 
-- Subtrees are also polymorphic 
data BinaryTree a = Leaf | 
                    Node a (BinaryTree a) (BinaryTree a)  

height :: BinaryTree -> Int 
height (Leaf) = 0
height (Node x left right) = 1 + max(height left)(height right)

--Use || because it both return false, we know its not in the tree
--If either return true, the result will be true 
inTree :: BinaryTree -> Int -> Bool 
inTree Leaf _ = False
inTree (Node x left right) val
  | x == val  = True 
  | otherwise = inTree(left val) || inTree(right val) 

-- Made addNode polymorphic
-- Everything the same except signature. Think about type classes needed. 
-- Added Ord because we need to be able to use > < with value of a for BST 
addNode :: (Ord a) => (BinaryTree a) -> a -> (BinaryTree a) 
addNode Leaf val = Node val Leaf Leaf
addNode (Node x left right) 
  | val > x   = Node x left (addNode right val)  
  | otherwise = Node x (addNode left val) right 

listToBST :: [Int] -> BinaryTree -> BinaryTree 
listToBST [] tree = tree 
listToBST (x:xs) tree = listToBST xs (addNode tree x) 

listToTreeF :: [Int] -> BinaryTree 
listToTreeF values = foldl addNode Leaf values  

treeMap :: (Int -> Int) -> BinaryTree -> BinaryTree 
treeMap _ Leaf = Leaf 
treeMap f (Node x left right) = Node (f x) (treeMap f left) (treeMap f right)   

-- Used Show so we could covert the a into a string for output 
-- inOrder goes from left subtree, to node, to right subtree 
inOrder :: (Show a) => (BinaryTree a) -> String
inOrder Leaf = "" 
inOrder (Node x left right) = (inOrder left) ++ (show x) ++ (inOrder right) 

primes :: Int -> [Int] 
primes n = sieve [2 .. n] 

sieve :: [Int] -> [Int] 
sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x /= 0] 
sieve [] = [] 

--When calling this function, to get first 100 primes use "take 100 primesInf"
--Can get the 10,000th prime using primes !! 10000 (only evaluates up to the 10,000th and evaluates nothing more)
primesInf :: [Int] 
primesInf = sieve [2 ..]