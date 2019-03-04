import POL
import Ratio

infixr 8  ^-

(^-) :: Integral a => a -> a -> a
x ^- 0 = 1
x ^- n = (x ^- (n-1)) * (x - n + 1)

infixr 8  ^+

(^+) :: Integral a => a -> a -> a
x ^+ 0 = 1
x ^+ n = (x ^+ (n-1)) * (x + n - 1)

newton :: (Fractional a, Enum a) => [a] -> [a]
newton xs = 
  [ x / product [1..fromInteger k] | (x,k) <- zip xs [0..]]

firstDifs :: [Integer] -> [Integer]
firstDifs xs = reverse $ map head (difLists [xs])

list2npol ::  [Integer] -> [Rational]
list2npol = newton . map fromInteger. firstDifs

stirlingC :: Integer -> Integer -> Integer               
stirlingC 0 0 = 1
stirlingC 0 _ = 0
stirlingC n k = (n-1) * (stirlingC (n-1) k) 
                 + stirlingC (n-1) (k-1)

fall2pol :: Integer -> [Integer]
fall2pol 0 = [1]
fall2pol n = 
   0 : [ (stirlingC n k) * (-1)^(n-k) | k <- [1..n] ]

npol2pol :: Num a => [a] -> [a]
npol2pol xs = 
  sum [ [x] * (map fromInteger $ fall2pol k) | 
                          (x,k) <- zip xs [0..] ]

list2pol :: [Integer] -> [Rational]
list2pol = npol2pol . list2npol

difference :: (Num a,Num b) => (a -> b) -> a -> b
difference f x = f (x+1) - f x

firstDfs :: (Num a,Num b) => (a -> b) -> [b]
firstDfs f = f 0 : firstDfs (difference f) 

