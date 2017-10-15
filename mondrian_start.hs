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
width :: Float
width = 1024

height :: Float
height = 768

border :: Float
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

--
-- Compute an integer between 120 and w from a (presumably random) floating
-- point number between 0 and 1.
--
shouldSplit :: Float -> Float -> Int
shouldSplit w r = if ((randomInt 120 (round(w*1.5)) r)<round(w)) then 1 else 0

-- Generate a tag for a random colored square given x y w h and a random float r between 0 and 1
makeRanSquare :: (Float, Float) -> (Float, Float) -> Float -> String
makeRanSquare (x,y) (w,h) r
  | r>=0.6 = makeSquare x y w h (0.3, 0.2, 0.3) -- place holder for white
  | r<0.6 && r>=0.4 = makeSquare x y w h (1, 0, 0) -- red
  | r<0.4 && r>=0.2 = makeSquare x y w h (0, 0, 1) -- blue
  | r<0.2 && r>=0.0 = makeSquare x y w h (1, 1, 0) -- yellow



-- Generate a tag for a square given x y w h and the R G B values as percentages
makeSquare :: Float -> Float -> Float -> Float -> (Float, Float, Float) -> String
makeSquare x y w h (r, g, b) = "<rect x=" ++ (show x) ++ 
       " y=" ++ (show y) ++ 
       " width=" ++ (show w) ++ 
       " height=" ++ (show h) ++ 
       " stroke=\"None\"" ++
       " fill=\"rgb(" ++ (show (round (r * 255))) ++ "," ++
                         (show (round (g * 255))) ++ "," ++
                         (show (round (b * 255))) ++ ")\" />\n" 

-- Generate a tag for a line given x1 y1 x2 y2 stroke(r,g,b) and stroke-width
makeLine :: (Float, Float) -> (Float, Float) -> (Float, Float, Float) -> Float -> String
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

mondrian :: Float -> Float -> Float -> Float -> [Float] -> ([Float], String)
mondrian x y w h (r:s:t:u:v:a:rs) 
   | w<0 || h<0 = (r:rs, "")
   |w>(width / 2) && h>(height / 2) =  (s:rs, quadSplit )   
   |w>(width / 2) && w>100 && (x<(width-100))  = (s:rs, vSplit) 
   |h>(height / 2) && h>100 && (y<(height-100)) = (s:rs, hSplit)  
   |(shouldSplit w r)==1 && (shouldSplit h r)==1 && (w)>200 && (h)>200 = (s:rs, quadSplit) 
   |(shouldSplit w r)==1 && (w)>200           = (s:rs, vSplit) 
   |(shouldSplit h r)==1 && (h)>200           = (s:rs, hSplit) 
   
   -- |(shouldSplit w r)==1 && (w)>200 && (h)>200 = (s:rs, quadSplit) 
   |otherwise = (r:rs, regSquare)
  
  where
   modifier = (border/2)
   ranWidth =  (w*0.5) -- x+(w*0.4) + (r*(w*0.2))
   ranHeight =  (h*0.5)
   canvasSquare = makeSquare x y w h (0,0,0) ++ makeSquare (x+border) (y+border) (w-(border*2)) (h-(border*2)) (0.5,0.5,0.5)
   regSquare = makeRanSquare (x+border,y+border) (w-border,h-border) r -- border is necessary since ranWidth and ranHeight can't return 0 but x y could be 0
   testLine = makeLine (0,0) (w,h) (12,12,11) 10   
   
   -- rV = (ranWidth-x)+(ranWidth+modifier) delete later
   -- vSplitLine = makeLine (x+ranWidth, y+border) (x+ranWidth, y+border+h) (122, 100, 122) border  
   
   ranVerLine = makeLine (x+ranWidth, y) (x+ranWidth, y+border+h) (122, 100, 122) border  
   ranHorLine = makeLine (x, y+ranHeight) (x+w, y+ranHeight) (1, 221, 1) border     
   
   upperLeft = snd (mondrian (x) (y) (ranWidth-modifier) (ranHeight-modifier) (s:rs) )
   upperRight = snd (mondrian (x+ranWidth-modifier) (y) (w-ranWidth+modifier) (ranHeight-modifier) (t:rs) ) 
   
   lowerRight = snd (mondrian (x+ranWidth-modifier) (y+ranHeight-modifier) (w-ranWidth+modifier) (h-ranHeight+modifier) (u:rs) )      
   lowerLeft = snd (mondrian (x) (y+ranHeight-modifier) (ranWidth-modifier)  (h-ranHeight+modifier) (v:rs) )   
   
   rightSplit = snd (mondrian (x+ranWidth) (y) ((ranWidth))(h) (r:rs)) 
   leftSplit = snd (mondrian (x) (y) (ranWidth-modifier-x) (h) (r:rs) )
   
   vSplit = ranVerLine  ++ rightSplit ++ leftSplit
   
   topSplit = snd (mondrian (x) (y) (w) (ranHeight-modifier-y) (r:rs) )
   btmSplit = snd (mondrian (x) (ranHeight+modifier+y) (w) (h-ranHeight-modifier) (u:rs) )   
   hSplit = ranHorLine  ++ btmSplit ++ topSplit
   
   quadSplit = ranVerLine ++ ranHorLine  ++ upperLeft    ++ lowerLeft ++  upperRight ++ lowerRight  
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