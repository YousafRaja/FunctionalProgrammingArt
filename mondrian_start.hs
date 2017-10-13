--
-- Starting code for CPSC 449 Assignment 1
-- 
-- Generate and output a Mondrian-style image as an SVG tag within an HTML 
-- document.
-- 
import System.IO
import Control.Monad (replicateM)
import System.Random (randomRIO, StdGen, randomR, mkStdGen)

--
-- The width and height of the image being generated.
-- 
width :: Int
width = 1024

height :: Int
height = 768

border :: Int
border = 20

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

-- Generate a tag for a random colored square given x y w h and a random float r between 0 and 1
makeRanSquare :: (Int, Int) -> (Int, Int) -> Float -> String
makeRanSquare (x,y) (w,h) r
  | r>=0.6 = makeSquare x y w h (0.3, 0.2, 0.3) -- place holder for white
  | r<0.6 && r>=0.4 = makeSquare x y w h (1, 0, 0) -- red
  | r<0.4 && r>=0.2 = makeSquare x y w h (0, 0, 1) -- blue
  | r<0.2 && r>=0.0 = makeSquare x y w h (1, 1, 0) -- yellow



-- Generate a tag for a square given x y w h and the R G B values as percentages
makeSquare :: Int -> Int -> Int -> Int -> (Float, Float, Float) -> String
makeSquare x y w h (r, g, b) = "<rect x=" ++ (show x) ++ 
       " y=" ++ (show y) ++ 
       " width=" ++ (show w) ++ 
       " height=" ++ (show h) ++ 
       " stroke=\"None\"" ++
       " fill=\"rgb(" ++ (show (round (r * 255))) ++ "," ++
                         (show (round (g * 255))) ++ "," ++
                         (show (round (b * 255))) ++ ")\" />\n" 

-- Generate a tag for a line given x1 y1 x2 y2 stroke(r,g,b) and stroke-width
makeLine :: (Int, Int) -> (Int, Int) -> (Int, Int, Int) -> Int -> String
makeLine (x1, y1) (x2, y2) (r, g, b) w = "<line x1=\"" ++ x1_s ++ "\" y1=\"" ++ y1_s ++ 
                                        "\" x2=\"" ++ x2_s ++ "\" y2=\"" ++ y2_s ++
                                        "\" style=\"stroke:rgb(" ++ rgb_s ++ ");stroke-width:" ++ w_s ++ "\"/>"
        where 
        x1_s = show (x1)
        y1_s = show (y1)
        x2_s = show (x2)
        y2_s = show (y2)
        w_s  = show (w)
        r_s  = show (r)
        b_s  = show (b)
        g_s  = show (g)
        rgb_s = show(r) ++ "," ++ show(g) ++ "," ++ show(b) 

--
-- Generate the tag for a rectangle with random color.  Replace the 
-- implementation of this function so that it generates all of the tags
-- needed for a piece of random Mondrian art.
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
 
--(snd (mondrian (ranWidth) (ranHeight) (w) (h) (s:rs)))
-- (ranWidth + (w-ranWidth)) (ranHeight + (h-ranHeight))
--   |w>(width `div` 2) && h>(height `div` 2) =  (s:rs, quadSplit ) 

mondrian :: Int -> Int -> Int -> Int -> [Float] -> ([Float], String)
mondrian x y w h (r:s:t:u:v:a:rs) 
  |w>(width `div` 2) && h>(height `div` 2) && w<=width =  (s:rs, vSplit )    
  |otherwise = if (x<w) then (r:rs, regSquare) else (r:rs, "")
  
  where
   modifier = (border `div` 2)
   ranWidth = x + (border `div` 2) + (round (r*(fromIntegral ((w-modifier)-x)))) 
   ranHeight = y + (border `div` 2) + (round (r*(fromIntegral (h-(border `div` 2)))))
   canvasSquare = makeSquare x y w h (0,0,0) ++ makeSquare (x+border) (y+border) (w-(border*2)) (h-(border*2)) (0.5,0.5,0.5)
   regSquare = makeRanSquare (x+border,y+border) (w-border,h-border) r -- border is necessary since ranWidth and ranHeight can't return 0 but x y could be 0
   testLine = makeLine (0,0) (w,h) (12,12,11) 10   
   
   ranVerLine = makeLine (ranWidth, y+border) (ranWidth, h) (122, 1, 1) border  
   ranHorLine = makeLine (x+border, ranHeight) (w, ranHeight) (1, 221, 1) border     
   
   upperLeft = snd (mondrian (x) (y) (ranWidth-modifier) (ranHeight-modifier) (s:rs) )
   upperRight = snd (mondrian (ranWidth-modifier) (y) (w-ranWidth+modifier) (ranHeight-modifier) (t:rs) )      
   lowerRight = snd (mondrian (ranWidth-modifier) (ranHeight-modifier) (w-ranWidth+modifier) (h-ranHeight+modifier) (u:rs) )      
   lowerLeft = snd (mondrian (x) (ranHeight-modifier) (ranWidth-modifier)  (h-ranHeight+modifier) (v:rs) )   
   
   rightSplit = snd (mondrian (ranWidth-modifier) (y) (w-(ranWidth-modifier))(h) (r:rs)) 
   leftSplit = snd (mondrian (x+border) (y+border) (ranWidth-modifier) (h) (s:rs) )
   
   vSplit = ranVerLine  ++ rightSplit -- ++ leftSplit
   
   topSplit = snd (mondrian (x+border) (y+border) (w-border*2) (ranHeight-modifier) (r:rs) )
   btmSplit = snd (mondrian (x+border) (ranHeight+border`div` 2) (w-border*2) (h-ranHeight-modifier) (u:rs) )   
   hSplit = ranHorLine ++ topSplit ++ btmSplit
   
   quadSplit = ranVerLine ++ ranHorLine ++ upperLeft ++ upperRight ++ lowerRight ++ lowerLeft 
-- fillSquare
 
  

--
-- The main program which generates and outputs mondrian.html.
--
main :: IO ()
main = do
  --  Right now, the program will generate a different sequence of random
  --  numbers each time it is run.  If you want the same sequence each time
  --  use "let seed = 0" instead of "seed <- randomRIO (0, 100000 :: Int)"

  --let seed = 0
  seed <- randomRIO (0, 100000 :: Int)
  let randomValues = randomList seed

  let prefix = "<html><head></head><body>\n" ++
               "<svg width=\"" ++ (show width) ++ 
               "\" height=\"" ++ (show height) ++ "\">"
      image = snd (mondrian 0 0 width height randomValues)
      suffix = "</svg>\n</html>"

  writeFile "mondrian.html" (prefix ++ image ++ suffix)