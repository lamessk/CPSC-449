data Head = Yellow | Green | Red | Black deriving Show 

data Screw = Robertson Head Float |
             Phillips Int Float   | 
             Slot Float Float deriving Show 

instance Eq Screw where
  s1 == s2 = (getLength s1) == (getLength s2)

getLength :: Screw -> Float 
getLength (Robertson _ len) = len
getLength (Phillips _ len) = len
getLength (Slot _ len) = len   