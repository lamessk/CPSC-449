--
-- Name: Lamess Kharfan
-- ID: 10150607 
--
-- CPSC 449 Spring 2018 Assignment 2
--
import qualified Data.ByteString.Lazy as BS
import Data.Word
import Data.Bits
import Data.Char
import Codec.Compression.Zlib as Z
import Numeric (showHex)

data Color = RGB Int Int Int deriving (Eq, Show)

-- 
-- Part 1: Create algebraic types for a quad tree and quad tree image
--

data QuadTree = Leaf Color | Node QuadTree QuadTree QuadTree QuadTree deriving (Eq, Show)

data QuadTreeImage = IMG Int QuadTree deriving Show 

--
-- Part 2: createTree takes a list representation of an image and stores it 
--         in a quad tree image. 
--

createTree :: [[Color]] -> QuadTreeImage  
createTree image 
  | (isSquare image) == False || (isPower2 0 fstrow) == False = error "Image is not square" 
  | otherwise                                                 = IMG width (createQTree image) 
  where 
    fstrow = head image 
    width  = length (fstrow)

--
-- createQTree: A recursive helper function that creates a quad tree for the quad tree image 
--              in Part 2.
--
createQTree :: [[Color]] -> QuadTree
createQTree img
  | (checkHomogenous img) = Leaf (head (head img)) 
  | otherwise             = Node (createQTree uLeft) (createQTree uRight) (createQTree lRight) (createQTree lLeft)
  where 
    fstrow = head img
    width  = length (fstrow)
    hlfW   = round ((fromIntegral width) / 2)
    uLeft  = take hlfW (map (take hlfW) img)
    uRight = take hlfW (map (drop hlfW) img) 
    lRight = drop hlfW (map (drop hlfW) img)
    lLeft  = drop hlfW (map (take hlfW) img)

--
-- isSquare: Checks if the image is square, returns a Boolean value to indicate weather or not it is. 
-- 
isSquare :: [[Color]] -> Bool 
isSquare cols 
  | length (head cols) == length cols = True 
  | otherwise                         = False 

--
-- isPower2: Check if the image's dimensions (height and width) are a power of 2
--           Returns a boolean to indicate weather or not the dimensions are a power of 2.
--
isPower2 :: Int -> [Color] -> Bool
isPower2 y w 
  | 2 ^ y  > length w = False 
  | 2 ^ y == length w = True 
  | otherwise         = isPower2 (y + 1) w 

--
-- checkHomogenous: A helper function for constructing the quad tree from a list of colors. 
-- CheckHomogenous checks if the region of the picture is one homogenous color. 
--
checkHomogenous :: [[Color]] -> Bool  
checkHomogenous []                                  = True 
checkHomogenous (x:[])
  | length x == length (filter ( ==head x) x)       = True
  | otherwise                                       = False   
checkHomogenous (x:xs)
  | (length x  == filtLen) && (firstC == secondC)        = checkHomogenous (xs)   
  | otherwise                                       = False 
  where 
    firstC  = head x
    secondC = head (head xs)
    filtLen = length (filter ( ==firstC) x)

-- 
-- Part 3: Determine how many internal nodes and how many leaf nodes there are in a quad tree. 
--         Counts represented by a tuple (Internal, Leaf) 
-- 
countNodes :: QuadTreeImage -> (Int, Int) 
countNodes (IMG w qt) = (countiNode qt, countlNode qt)  

countiNode :: QuadTree -> Int 
countiNode (Leaf _) = 0
countiNode (Node qt1 qt2 qt3 qt4) = 1 + (countiNode qt1) + (countiNode qt2) + (countiNode qt3) + (countiNode qt4)

countlNode :: QuadTree -> Int 
countlNode (Leaf _) = 1
countlNode (Node qt1 qt2 qt3 qt4) = (countlNode qt1) + (countlNode qt2) + (countlNode qt3) + (countlNode qt4)

--
-- Part 4: Genrate HTML SVG tags necassary to render a quad tree presentation of an 
--         image in a browser by generating a <rect> tag for every lead node in the tree. 
-- 
toHTML :: QuadTreeImage -> String 
toHTML (IMG w qt) = prefix ++ generateTag qt w (0,0) ++ suffix  
  where 
    prefix = "<html><head></head><body>\n" ++
             "<svg width=\"" ++ (show w) ++ 
             "\" height=\"" ++ (show w) ++ "\">"
    suffix = "</svg>\n</html>"

generateTag :: QuadTree -> Int -> (Int, Int) -> String 
generateTag (Leaf (RGB r g b)) w (x, y) =  "<rect x=" ++ (show x) ++ 
                                           " y=" ++ (show y) ++ 
                                           " width=" ++ (show w) ++ 
                                           " height=" ++ (show w) ++ 
                                           " stroke=\"None\"" ++
                                           " fill=\"rgb(" ++ (show r) ++ "," ++
                                                             (show g) ++ "," ++
                                                             (show b) ++ ")\" />\n"
generateTag (Node qt1 qt2 qt3 qt4) w (x,y) =
                                           generateTag qt1 w2 (x,y) ++ 
                                           generateTag qt2 w2 (x + w2, y) ++
                                           generateTag qt3 w2 (x + w2, y + w2) ++
                                           generateTag qt4 w2 (x, y + w2)
  where 
    w2 = w `div` 2

--
-- Part 5: merge two quad tree images. Result is a new image where overlapping pixels 
--         have been merged using a function given as one of the parameters to merge. 
-- 
merge :: QuadTreeImage -> QuadTreeImage -> (Color -> Color -> Color) -> QuadTreeImage
merge (IMG w1 qt1) (IMG w2 qt2) f 
  | w1 /= w2  = error "Images being merged are different sizes"
  | otherwise = (IMG w1 (mergeImages qt1 qt2 f))
  
mergeImages :: QuadTree -> QuadTree -> (Color -> Color -> Color) -> QuadTree 
mergeImages (Node qt1 qt2 qt3 qt4) (Node qt5 qt6 qt7 qt8) f = Node (mergeImages qt1 qt5 f)
                                                                   (mergeImages qt2 qt6 f) 
                                                                   (mergeImages qt3 qt7 f) 
                                                                   (mergeImages qt4 qt8 f)
mergeImages (Leaf col1) (Leaf col2)  f                       = Leaf (f col1 col2)
mergeImages x (Node qt1 qt2 qt3 qt4) f                       = Node (mergeImages x qt1 f)
                                                                   (mergeImages x qt2 f) 
                                                                   (mergeImages x qt3 f) 
                                                                   (mergeImages x qt4 f)
mergeImages (Node qt1 qt2 qt3 qt4) x f                      = Node (mergeImages qt1 x f)
                                                                   (mergeImages qt2 x f) 
                                                                   (mergeImages qt3 x f) 
                                                                   (mergeImages qt4 x f)
--
-- Part 6: optimize a quad tree image given by the merge function that detects cases where
--         an internal node has four identical children and redcues them to a single leaf node.

--
optimizeImage :: QuadTreeImage -> QuadTreeImage 
optimizeImage (IMG w qt) = (IMG w (optHelper qt))

optHelper :: QuadTree -> QuadTree
optHelper (Leaf c) = (Leaf c) 
optHelper (Node qt1 qt2 qt3 qt4) 
  | eql = r1 
  | otherwise = Node r1 r2 r3 r4
  where 
    r1 = optHelper qt1 
    r2 = optHelper qt2 
    r3 = optHelper qt3 
    r4 = optHelper qt4 
    eql = (r1 == r2) && (r2 == r3) && (r3 == r4) 

--
--  Merge two colors by computing their average.  This makes it look like the
--  two images being merged are 'ghosted' on top of each other.
--
average :: Color -> Color -> Color
average (RGB r1 g1 b1) (RGB r2 g2 b2) = (RGB r_avg g_avg b_avg)
  where
    r_avg = (r1 + r2) `div` 2
    g_avg = (g1 + g2) `div` 2
    b_avg = (b1 + b2) `div` 2

--
--  Merge two colors by retaining the brighter color.  This allows us to take
--  an image with black regions in it and replace them with a texture from
--  another image.
--
brightest :: Color -> Color -> Color
brightest (RGB r1 g1 b1) (RGB r2 g2 b2)
  | brightness1 > brightness2 = (RGB r1 g1 b1)
  | otherwise = (RGB r2 g2 b2)
  where
    brightness1 = r1 + g1 + b1
    brightness2 = r2 + g2 + b2

--
--  Merge two colors by retaining the darker color.  This allows us to 
--  overlay dark text / pixels on top of an existing image.
--
darkest :: Color -> Color -> Color
darkest (RGB r1 g1 b1) (RGB r2 g2 b2)
  | brightness1 < brightness2 = (RGB r1 g1 b1)
  | otherwise = (RGB r2 g2 b2)
  where
    brightness1 = r1 + g1 + b1
    brightness2 = r2 + g2 + b2

--
-- Load a pair of PNG files, convert them into quad tree images, compute a
-- merged image, optimize it and write all four images to an .html file.
--
main :: IO ()
main = do
  -- Change the names inside double quotes to load different files
  input_1 <- BS.readFile "blocks_3.png"
  input_2 <- BS.readFile "blocks_4.png"
 
  -- image_n is the list representation of the image stored in the .png file
  let image_1 = decodeImage input_1
  let image_2 = decodeImage input_2

  -- Convert the list representation of each image into a tree representation
  let qtree_image_1 = createTree image_1
  let qtree_image_2 = createTree image_2
  
  writeFile "tree.txt" ((show qtree_image_1) ++ "\n\n" ++ (show qtree_image_2))

  -- Compute the number of nodes in each of the loaded images
  let (interior_1, leaf_1) = countNodes qtree_image_1
  let (interior_2, leaf_2) = countNodes qtree_image_2

  -- Merge the images
  --
  -- You can replace the call to average at the end of this line with
  -- a different function like brightest or darkest, or with any other
  -- function that combines two colors that you write yourself.
  --
  let merged = merge qtree_image_1 qtree_image_2 average
  let (interior_m, leaf_m) = countNodes merged

  -- Optimize the merged images
  let optimized = optimizeImage merged
  let (interior_o, leaf_o) = countNodes optimized
 
  writeFile "quadtree.html" ("<p>Merge Image 1:<p>\n" ++
                            (toHTML qtree_image_1) ++ 
                             "<p>Interior Nodes: " ++ show interior_1 ++
                             "<br>Leaf Nodes: " ++ show leaf_1 ++ 
                             "<br><br><br><br>" ++

                             "<p>Merge Image 2:<p>\n" ++ 
                            (toHTML qtree_image_2) ++ 
                             "<p>Interior Nodes: " ++ show interior_2 ++
                             "<br>Leaf Nodes: " ++ show leaf_2 ++ 
                             "<br><br><br><br>" ++

                             "<p>Merge Result:<p>\n" ++ 
                             (toHTML merged) ++ "<p>"  ++
                             "<p>Interior Nodes: " ++ show interior_m ++
                             "<br>Leaf Nodes: " ++ show leaf_m ++ 
                             "<br><br><br><br>" ++

                             "<p>Optimized Result:<p>\n" ++ 
                             (toHTML optimized) ++ "<p>"  ++
                             "<p>Interior Nodes: " ++ show interior_o ++
                             "<br>Leaf Nodes: " ++ show leaf_o ++ 
                             "<br><br><br><br>"
                            )

-------------------------------------------------------------------------------
--
--  DO NOT MODIFY THE CODE BELOW THIS POINT IN THE FILE.  
--
-------------------------------------------------------------------------------
--
-- The following functions are a simple PNG file loader.  Note that these
-- functions will not load all PNG files.  They makes some assumptions about
-- the structure of the file that are not required by the PNG standard,
-- don't support transparency, and only support a limited number of bit
-- depths.  Hopefully they will generate an error message if you try and load 
-- an unsupported file.  Hopefully...
--

--
-- Convert 4 8-bit words to a 32 bit (or larger) integer
--
make32Int :: Word8 -> Word8 -> Word8 -> Word8 -> Int 
make32Int a b c d = ((((fromIntegral a) * 256) + 
                       (fromIntegral b) * 256) + 
                       (fromIntegral c) * 256) + 
                       (fromIntegral d)

--
-- Get a list of all of the PNG blocks out of a list of bytes
--
getBlocks :: [Word8] -> [(String, [Word8])]
getBlocks [] = []
getBlocks (a:b:c:d:e:f:g:h:xs) = (name, take (size+12) (a:b:c:d:e:f:g:h:xs)) : getBlocks (drop (size + 4) xs)
  where
    size = make32Int a b c d
    name = (chr (fromIntegral e)) : (chr (fromIntegral f)) :
           (chr (fromIntegral g)) : (chr (fromIntegral h)) : []

--
-- Extract the information out of the IHDR block
--
getIHDRInfo :: [(String, [Word8])] -> (Int, Int, Int, Int)
getIHDRInfo [] = error "No IHDR block found"
getIHDRInfo (("IHDR", (_:_:_:_:_:_:_:_:w1:w2:w3:w4:h1:h2:h3:h4:bd:ct:_)) : _) = (make32Int w1 w2 w3 w4, make32Int h1 h2 h3 h4, fromIntegral bd, fromIntegral ct)
getIHDRInfo (x : xs) = getIHDRInfo xs

--
-- Extract and decompress the data in the IDAT block(s).  
--
getImageData :: [(String, [Word8])] -> [Word8]
getImageData blocks 
  | idat_blocks == [] = error "No IDAT block found"
  | otherwise = decompressed_data
  where 
    idat_blocks = filter (\(x, _) -> x == "IDAT") blocks
    idat_datas = map (\(x, (_:_:_:_:_:_:_:_:xs)) -> take (length xs - 4) xs) idat_blocks
    merged_data = foldl1 (++) idat_datas
    decompressed_data = BS.unpack (Z.decompress (BS.pack merged_data))

--
-- Convert a list of bytes to a list of Color
--
makeColors :: [Word8] -> [Color]
makeColors [] = []
makeColors (r : g : b : vals) = RGB (fromIntegral r) (fromIntegral g) (fromIntegral b) : makeColors vals

--
-- Convert a list of bytes that have been decompressed from a PNG file into
-- a two dimensional list representation of the image
--
imageBytesToImageList :: [Word8] -> Int -> [[Color]]
imageBytesToImageList [] _ = []
imageBytesToImageList (_:xs) w = makeColors (take (w * 3) xs) : imageBytesToImageList (drop (w * 3) xs) w

--
-- Determine how many IDAT blocks are in the PNG file
--
numIDAT :: [(String, [Word8])] -> Int
numIDAT vals = length (filter (\(name, dat) -> name == "IDAT") vals)

--
-- Determine if there is a cHRM block in the PNG file
--
hasCHRM :: [(String, [Word8])] -> Bool
hasCHRM vals = (length (filter (\(name, dat) -> name == "cHRM") vals)) /= 0

--
-- Convert the entire contents of a PNG file (as a ByteString) into 
-- a two dimensional list representation of the image
--
decodeImage :: BS.ByteString -> [[Color]]
decodeImage bytes
  | hasCHRM blocks = error "Image contains a CHRM chunk which this loader is unable to handle"
  | header == [137,80,78,71,13,10,26,10] &&
    colorType == 2 &&
    bitDepth == 8 = imageBytesToImageList imageBytes w
  | otherwise = error ("Invalid header\ncolorType: " ++ (show colorType) ++ "\nbitDepth: " ++ (show bitDepth) ++ "\n")
  where
    header = take 8 $ BS.unpack bytes
    (w, h, bitDepth, colorType) = getIHDRInfo blocks
    imageBytes = getImageData blocks
    blocks = getBlocks (drop 8 $ BS.unpack bytes)

--
--  Get all of the blocks (chunks) from a ByteString
--
getImageBlocks :: BS.ByteString -> [(String, [Word8])]
getImageBlocks bytes = getBlocks (drop 8 $ BS.unpack bytes)

-------------------------------------------------------------------------------
--
--  END OF PNG LOADER CODE
--
-------------------------------------------------------------------------------