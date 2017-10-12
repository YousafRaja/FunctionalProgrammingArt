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
  | r>=0.6 = makeSquare x y w h (1, 1, 1)
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
mondrian :: Int -> Int -> Int -> Int -> [Float] -> ([Float], String)
mondrian x y w h (r:s:t:u:rs) 
  |w==width && h==height =  (s:rs, canvasSquare ++ quadSplit ++ 
  (snd (mondrian (x) (y) (ranWidth) (ranHeight) (r:rs))) ++ 
  (snd (mondrian (ranWidth+(border `div` 2)) (y) ( w-(ranWidth)-(border*2) ) (ranHeight)  (s:rs))) ++ 
  (snd (mondrian (ranWidth+(border `div` 2)) (ranHeight +(border `div`2)) ( w-(ranWidth)-(border*2) ) (h-(ranHeight)-(border*2))  (t:rs))) ++ 
  (snd (mondrian (x) (ranHeight +(border `div`2)) (ranWidth) (h-(ranHeight)-(border*2))  (u:rs)))   ) 
  |otherwise = (s:rs, regSquare)  
  
  where
   ranWidth = (round (r*(fromIntegral w))) - (border*2)
   ranHeight = (round (r*(fromIntegral h))) - (border*2)
   canvasSquare = makeSquare x y w h (0,0,0) ++ makeSquare (x+border) (y+border) (w-(border*2)) (h-(border*2)) (0.5,0.5,0.5)
   regSquare = makeRanSquare (x+border,y+border) (w-(border `div` 2),h-(border `div` 2)) r 
   testLine = makeLine (0,0) (w,h) (12,12,11) 10   
   ranVerLine = makeLine (ranWidth+border, y+border) ((round (r*(fromIntegral w)))-border, h-border) (122, 1, 1) border  
   ranHorLine = makeLine (x+border, ranHeight+border) (w-border, (round (r*(fromIntegral h)))-border) (1, 221, 1) border  
   quadSplit = ranVerLine ++ ranHorLine
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