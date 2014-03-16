module Euler where
import qualified Data.Set as S

fibs :: (Num a) => [a]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

primesTill :: (Integral a) => a -> [a]
primesTill m = 2 : sieve [3, 5 .. m]
    where
        sieve (p:xs)
            | p * p > m = p : xs
            | otherwise = p : sieve [x | x <- xs, rem x p /= 0]

divides :: (Integral a) => a -> a -> Bool
n `divides` k = n `mod` k == 0

divisors :: (Integral a) => a -> [a]
divisors n = little ++ (check . reverse) bigs
    where little = filter (\x -> mod n x == 0) [1 .. floor $ sqrt $ fromIntegral n]
          bigs = map (n `quot`) little
          sortaN = fromIntegral n
          check (x:xs) = if x * x == n
                            then xs
                            else (x:xs)

factorial :: (Integral a, Enum a) => a -> a
factorial n = product [1..n]

choose :: (Integral a) => a -> a -> a
choose n k = factorial n `quot` ((factorial k) * (factorial (n - k)))

square :: (Num a) => a -> a
square x = x * x

d :: (Integral a) => a -> a
d n = sum (divisors n) - n

isPermOf :: (Eq a, Ord a) => [a] -> S.Set a -> Bool
isPermOf xs s = S.fromList xs == s && isPerm xs

isPerm :: (Eq a) => [a] -> Bool
isPerm [] = True
isPerm (x:xs) = (x `notElem` xs) && isPerm xs
