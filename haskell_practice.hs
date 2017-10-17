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
  
 
doubleAll :: [Integer] -> [Integer]
doubleAll l = [ 2*e | e <- l ]
 

divides x y = x `mod` y == 0
divisors :: Integer -> [Integer]
divisors n = [ y | y <- [1..n], divides n y ]
 