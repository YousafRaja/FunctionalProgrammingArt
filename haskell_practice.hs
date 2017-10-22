
counter :: Integer -> Integer -> [Integer]
counter n i = [e | e<-[i..n], e `mod` 2 == 0] 

-- Chapter 5

maxOccurs :: Integer -> Integer -> (Integer,Integer)
maxOccurs a b
  | a == b = (m, 2)
  | otherwise = (m, 1)
  where m = max a b

maxThreeOccurs:: Integer -> Integer -> Integer -> (Integer, Integer)
maxThreeOccurs a b c
  | max_ab < c = (max a b, 1)
  | max_ab == c = (a, 3)
  | max_ab > c = (max_ab, occ_ab)
  where 
  max_ab = fst m
  occ_ab = snd m
  m = maxOccurs a b  
  
 
divides x y = if (x `mod` y == 0) then 1 else 0 
divisors:: Integer -> [Integer]
divisors x = [ y | y<-[1..x], x `divides` y == 1]


matches :: Integer -> [Integer] -> [Integer]
matches n l = [ x | x<-l, x == n]  

eleM :: Integer -> [Integer] -> Bool
eleM x l = length (matches x l) > 0

-- recursive
duplicateR :: String -> Integer -> String
duplicateR s 0 = ""
duplicateR s 1 = s 
duplicateR s n = duplicateR s (n-1) ++ s

-- list comprehension
joinStrings :: [String] -> String
joinStrings ss = [ c | s <- ss, c <- s ]

duplicateL :: String -> Integer -> String
duplicateL s n = joinStrings [s | a <- [0 .. n-1] ] 




-- Chp 6 

superImpose :: Char -> Char -> Char
superImpose '.' '.' = '.'
superImpose _ _ = '#'

superImposeLine  :: [Char] -> [Char] -> [Char]
superImposeLine a b = [superImpose x y | (x,y) <- zip a b]




   