--
--  Starting code for Final Exam Question #2
--
data Color = RGB Int Int Int deriving (Eq, Show)
data QTImage = Image Int Tree deriving Show
--
--  Assume that the children are ordered upper-left, upper-right, lower-right,
--  lower-left
data Tree = Leaf Color | Node Tree Tree Tree Tree deriving Show

white = RGB 255 255 255
black = RGB 0 0 0 
red   = RGB 255 0 0 
green = RGB 0 255 0
blue  = RGB 0 0 255

-- A 16x16 image that is all white.
i1 = Image 16 (Leaf white)

-- A 16x16 image where the upper left and lower right quadrants are white 
-- and the upper right and lower left quarters are black
i2 = Image 16 (Node (Leaf white) (Leaf black) (Leaf white) (Leaf black))

-- A 32x32 image with a more complex structure
i3 = Image 32 (Node (Leaf white) 
                    (Node (Leaf white) (Leaf black) (Leaf white) (Leaf black))
                    (Leaf black)
                    (Node (Leaf white) (Leaf black) (Leaf white)
                      (Node (Leaf white) (Leaf black) (Leaf white)
                        (Node (Leaf white) (Leaf black) (Leaf white) (Leaf red))
                      )
                    )
              )

-- Another 32x32 image with a more complex structure
i4 = Image 32 (Node (Leaf red) 
                    (Node (Leaf white) (Leaf black) (Leaf white) 
                      (Node (Leaf white) (Leaf black) (Leaf white)
                        (Node (Leaf white) (Leaf black) (Leaf white)
                          (Node (Leaf white) (Leaf black) (Leaf white) (Leaf green)
                          )
                        )
                      )
                    )
                    (Leaf black)
                    (Node (Leaf white) (Leaf black) (Leaf white)
                      (Node (Leaf white) (Leaf black) (Leaf white)
                        (Node (Leaf white) (Leaf black) (Leaf white) (Leaf red))
                      )
                    )
              )
------------------------------------------------------
------------------- Answer to Q2 ---------------------
------------------------------------------------------

numPixels :: QTImage -> Color -> Int 
numPixels (Image w qt) col = (numPixelsHelp qt col) * w * w

numPixelsHelp :: Tree -> Color -> Int 
numPixelsHelp (Leaf c) col 
  | c == col = 1
  | otherwise = 0
numPixelsHelp (Node qt1 qt2 qt3 qt4) col = (numPixelsHelp qt1 col) + (numPixelsHelp qt2 col) + (numPixelsHelp qt3 col)  + (numPixelsHelp qt4 col)
















