-- Add two fractions together 
addFractions :: (Int, Int) -> (Int, Int)  -> (Int, Int) 
addFractions (a, b) (c, d) = if (b == 0 || d == 0) then (1, 0) else (((a * d) + (b * c)),(b * d))  