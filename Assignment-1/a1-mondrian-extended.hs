-- Lamess Kharfan 
-- CPSC 449 - Assignment 1
--
-- Generate and output a Mondrian-style image as an SVG tag within an HTML 
-- document with added artistic element.
-- Artistic element: Randomly choosing line widths based on location across the width. 
--                   Random coloring in the range of reds, pinks and yellows, and oranges. 
--
import System.IO
import Control.Monad (replicateM)
import System.Random (randomRIO, StdGen, randomR, mkStdGen)

--
-- The width and height of the image being generated.
width :: Int
width = 1024

height :: Int
height = 768

--
-- Generate and return a list of 20000 random floating point numbers between 
-- 0 and 1.  (Increase the 20000 if you ever run out of random values).
-- 
randomList :: Int -> [Float]
randomList seed = take 20000 (rl_helper (mkStdGen seed))

rl_helper :: StdGen -> [Float]
rl_helper g = fst vg : rl_helper (snd vg)
  where vg = randomR (0.0, 1.0 :: Float) g

--
-- Compute an integer between low and high from a (presumably random) floating
-- point number between 0 and 1.
--
randomInt :: Int -> Int -> Float -> Int
randomInt low high x = round ((fromIntegral (high - low) * x) + fromIntegral low)

--
-- Generate and SVG tag for a rectangle with color 
-- Parameters: x, y: coordinates of upper left corner of the rectangle
--             w, h: width and height of the rectangle to be drawn 
--             r:s:t:rs: a list of random ints to generate line width and color 
-- Returns: String that is an SVG tag for the rectangle to be drawn 
--   
drawRectangle :: Int -> Int -> Int -> Int -> [Float] -> String
drawRectangle x y w h (r:s:t:rs) =  "<rect x=" ++ (show x) ++ " y=" ++ (show y) ++ 
                                    " width=" ++ (show w) ++ " height=" ++ (show h) ++ 
                                    " stroke=\"black\"" ++ " stroke-width=\"" ++ (show l_width) ++ "\"" ++
                                    " fill=\"rgb(" ++ (show red)   ++ "," 
                                                   ++ (show green) ++ "," 
                                                   ++ (show blue)  ++ ")\" />\n)"
  where 
    l_width = selectLineWidth x r  
    red = randomInt 240 255 r
    green = randomInt 0 200 s
    blue = randomInt 0 255 t

--
-- Function that decides what the width of the lines will be.
-- Uses value of x coordinate and random value r to decide width randomly 
-- Parameters: x: the x-coordinate of the upper right corner of the rectangle 
--             r: a random float between 0 and 1 
-- Returns: An integer value representing thickness of the line to draw 
--
selectLineWidth :: Int -> Float -> Int 
selectLineWidth x r 
  | x <  third_w    && r < 0.20 = 9
  | x <  twothird_w && r < 0.40 = 7
  | x >= twothird_w && r < 0.60 = 5
  | x >= twothird_w || r < 0.90 = 3
  | otherwise                   = 1
  where 
    third_w    = round (fromIntegral(width) * 0.36)
    twothird_w = round (fromIntegral(width) * 0.67)

--
-- Generate Mondrian-style art using SVG tags for rectangles. 
-- 
-- Parameters:
--   x, y: The upper left corner of the region
--   w, h: The width and height of the region
--   r:s:t:rs: A list of random floating point values between 0 and 1
--
-- Returns:
--   [Float]: The remaining, unused random values
--   String: The SVG tags that draw the image
--
mondrian :: Int -> Int -> Int -> Int -> [Float] -> ([Float], String)
mondrian x y w h (r:s:t:rs) 
  | (w > half_width) && (h > half_height)                = (lr_rand, ul_tags ++ ur_tags ++ ll_tags ++ lr_tags)
  | (w > half_width)                                     = (r_rand, left ++ right)
  | (h > half_height)                                    = (b_rand, top ++ bottom) 
  | (dec_spl_v) && (dec_spl_h) && (w > max) && (h > max) = (lr_rand, ul_tags ++ ur_tags ++ ll_tags ++ lr_tags)
  | (dec_spl_v) && (w > max)                             = (r_rand, left ++ right)
  | (dec_spl_h) && (h > max)                             = (b_rand, top ++ bottom) 
  | otherwise                                            = (rs, drawRectangle x y w h [r, s, t])
  where 
    -- half height and new width original canvas 
    half_width  = (width `div` 2) 
    half_height = (height `div` 2)

    -- 4 recursive calls to make for spliting vertically and horizontally 
    (ul_rand, ul_tags) = (mondrian x y rand_v rand_h rs)
    (ur_rand, ur_tags) = (mondrian (x + rand_v) y (w - rand_v) rand_h ul_rand)
    (ll_rand, ll_tags) = (mondrian x (rand_h + y) (rand_v) (h - rand_h) ur_rand)
    (lr_rand, lr_tags) = (mondrian (rand_v + x) (rand_h + y) (w - rand_v) (h - rand_h) ll_rand)

    -- 2 calls to split vertically with 2 rectangles on the left and right 
    (l_rand, left)  = (mondrian x y rand_v h rs)
    (r_rand, right) = (mondrian (x + rand_v) y (w - rand_v) h l_rand) 

    -- 2 calls to split horizontally with 2 rectangles on the top and bottom 
    (t_rand, top)    = (mondrian x y w rand_h rs)
    (b_rand, bottom) = (mondrian x (rand_h + y) w (h -rand_h) t_rand) 

    -- Randomly choose split point for height and width 
    rand_v   = getSplitPoint w r
    rand_h   = getSplitPoint h s

    -- Deciding weather or not to split 
    (d_rand, dec_spl_v) = (splitRegion w rs)
    (c_rand, dec_spl_h) = (splitRegion h d_rand)

    --Variable names 
    max = 120

--
-- Random generate a split point between 33% and 66% of the given height or width 
-- Parameters: a = height or width, b = random float to pass to RandomInt 
-- Returns: A random Integer between 33% and 66% of given width/height 
-- 
getSplitPoint :: Int -> Float -> Int 
getSplitPoint a b =  randomInt bound_1 bound_2 b 
  where 
    bound_1 = round (fromIntegral(a) * onethird)
    bound_2 = round (fromIntegral(a) * twothird)
    onethird = 0.33
    twothird = 0.66

--
-- A function that randomly decides weather or not to split a region 
-- Parameters: Integer a = height or width of the region, List of Random floats
-- Returns: Boolean indicating weather or not the split the region based on calculation using random integer
--
splitRegion :: Int -> [Float] -> ([Float], Bool)
splitRegion a (r:rs)
  | split_reg < a        = (rs, True)
  | otherwise            = (rs, False) 
  where 
    num       = round (fromIntegral(a) * 1.5)
    split_reg = randomInt 120 num r
--
-- The main program which generates and outputs mondrian.html.
--
main :: IO ()
main = do
  seed <- randomRIO (0, 100000 :: Int)
  let randomValues = randomList seed

  let prefix = "<html><head></head><body>\n" ++
               "<svg width=\"" ++ (show width) ++ 
               "\" height=\"" ++ (show height) ++ "\">"
      image = snd (mondrian 0 0 width height randomValues)
      suffix = "</svg>\n</html>"

  writeFile "mondrian.html" (prefix ++ image ++ suffix)