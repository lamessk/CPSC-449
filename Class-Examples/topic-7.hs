-- Create a new data type for primary color 
-- 3 possible values, red green or blue, must be captials 
-- Add deriving show so that it'll print the value to the screen 
-- Use (deriving (Eq, Show) to get both 
data PrimaryColor = Red | Green | Blue deriving Show


--Days of the week 
-- Add deriving Eq to try to compare if two values are equal 
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving Eq

-- An instance of how show works for the Day data type using pattern matching 
--Don't put Deriving show into the derving part of day because it'll get confused about if you're trying
-- to use default show or the one you defined and throw an error 
instance Show Day where 
  show Monday = "Mon"
  show Tuesday = "Tues" 
  show Wednesday = "Wed" 
  show Thursday = "Thurs" 
  show Friday = "Fri"
  show Saturday = "Sat"
  show Sunday = "Sun"

-- Any type we define can be used like we would any other pre-defined types like integer or String 
isWeekend :: Day -> Bool 
isWeekend Saturday = True 
isWeekend Sunday   = True 
isWeekend _        =  False 

data RGBColor = RGB Int Int Int deriving Show 

-- Product type example
data Shape = Circle Float | 
             Rectangle Float Float |
             Triangle Float Float Float deriving Show

-- Use pattern matching to match 3 different constructors to shape 
-- Enclose constructor in parentheses when we arepattern matching against more than 1 thing (list we do with lists) 
area :: Shape -> Float 
area (Circle radius)  = 3.14 * radius 
area (Rectangle w h)  = w * h
area (Triangle a b c) = sqrt(p * (p - a) * (p - b) * (p - c))
  where 
    p = (a + b + c)/2

-- If we use show eq in the data shape then it'll show that 2 shapes are equal if their parameters are the same 
-- sometimes this doesnt meet our def of equal. some might say a 3x2 rectangle isn't the same as 2x3 (Normal Eq)
--  but maybe we want to say it is the same 
--
-- Two shapes are equal if their areas are within 0.001 of eachother (Problems can arise if we have 2 different shapes with same area)
-- Can try to make this such that the shapes are the same AND the areas are within 0.001th of eachother
-- 
instance Eq Shape where 
  s1 == s2 = abs (area s1 - area s2) < 0.001 

 -- This is a way to try to do it such that shapes are the same AND area's the same 
--instance Eq Shape where 
--  Circle r1) == (Circle r2) = abs (area (Circle r1) = area (Circle r2)) < 0.001
  -- Now define for other 2 shapes 
-- _ == _ = False -- Make sure to catch all possible patterns 











