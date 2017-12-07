import Euler
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Char
import Data.List
import Control.Monad
import Data.Ratio
import Control.Arrow
import Data.Maybe
import Data.Bits
import Debug.Trace
{-# LANGUAGE TupleSections #-}
-- fiftyone
--
--
--
data Bit = Zero | One deriving (Show, Eq, Ord)

rdList :: Int -> [Int]
rdList 0 = []
rdList n = (n `mod` 10) : rdList (n `quot` 10)
dList = reverse . rdList

rreNum :: [Int] -> Int
rreNum [] = 0
rreNum (x:xs) = x + 10 * rreNum xs
reNum = rreNum . reverse

replace :: Int -> [Bit] -> [Int]
replace x mask = do
    k <- [0..9]
    return (reNum . sub k mask . dList $ x)

sub :: Int -> [Bit] -> [Int] -> [Int]
sub _ [] [] = []
sub k (One:bs) (x:xs) = k : (sub k bs xs)
sub k (Zero:bs) (x:xs) = x : (sub k bs xs)
sub _ _ _ = undefined


genMasks :: Int -> [[Bit]]
genMasks 0 = [[]]
genMasks n = (map (Zero:) . genMasks $ n - 1) ++ (map (One:) . genMasks $ n - 1)

-- This is partially lying, because 109 is in the results. According to the example,
-- leading 0 digits don't count: *3 -> [1-9]3, even though 56**3 -> 56[0-9]{2}3
-- (it also takes about a half hour)
fiftyone = do
    p <- primesTil 1000000
    m <- genMasks . ceiling . logBase 10 . fromIntegral $ p
    let ps = S.fromList . filter isPrime . replace p $ m
    guard $ S.size ps == 8
    return ps

-- fiftytwo: inspection. 1 / 7.0 = 0.142857
--
-- fiftythree
-- 23 10 -> 13
-- 24 10
fiftythree n = sum $ do
    x <- [1..n]
    m <- take 1 . filter ((>= 1000000) . (x `choose`)) $ [0..x]
    return . max 0 $ (x - 2 * m + 1)

-- fiftyfour
data Suit = H | C | S | D deriving (Eq, Show, Read, Ord)
data Count = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack| Queen | King | Ace deriving (Eq, Ord, Show, Enum)


instance Read Count where
    readsPrec _ (x:xs)
        | x >= '2' && x <= '9' = [(toEnum $ ord x - ord '2', xs)]
        | x == 'T' = [(Ten, xs)]
        | x == 'J' = [(Jack, xs)]
        | x == 'Q' = [(Queen, xs)]
        | x == 'K' = [(King, xs)]
        | x == 'A' = [(Ace, xs)]
        | otherwise = error . show $ x
    readsPrec _ [] = undefined

data Card = Card Count Suit deriving (Eq, Show, Ord)

count :: Card -> Count
count (Card c _) = c

suit :: Card -> Suit
suit (Card _ s) = s

instance Read Card where
    readsPrec _ (x:y:xs) = [(Card (read [x]) (read [y]), xs)]

sameSuit :: [Card] -> Bool
sameSuit (h:hs)= isJust . foldr match (Just . suit $ h) . map (Just . suit) $ hs
    where match (Just a) (Just b) | a == b = Just a
                                  | otherwise = Nothing
          match _ _ = Nothing

isStraight :: [Card] -> Bool
isStraight = fld . sort . map count
    where fld :: [Count] -> Bool
          fld hand@(_:xs) = and . zipWith (==) xs . map succ $ hand

data Score = HighCard Count Count Count Count Count
           | OnePair Count Count Count Count
           | TwoPairs Count Count Count
           | ThreeOfAKind Count Count Count
           | Straight Count
           | Flush Count Count Count Count Count
           | FullHouse Count Count
           | FourOfAKind Count Count
           | StraightFlush Count -- StraightFlush Ace is a Royal flush
    deriving (Show, Eq, Ord)

straightFlush :: [Card] -> Maybe Score
straightFlush hand 
    | isStraight hand && sameSuit hand = Just . StraightFlush . maximum . map count $ hand
    | otherwise = Nothing

fourOfAKind :: [Card] -> Maybe Score
fourOfAKind = helper . sort . map count 
    where helper (a:b:c:d:e:[]) 
            | and . map (==a) $ [b, c, d] = Just . FourOfAKind a $ e
            | and . map (==e) $ [b, c, d] = Just . FourOfAKind e $ a
            | otherwise = Nothing
          helper x = error . show $ x

fullHouse :: [Card] -> Maybe Score
fullHouse = helper . sort . map count
    where helper (a:b:c:d:e:[])
            | a == b && b == c && d == e = Just $ FullHouse a e
            | a == b && c == d && d == e = Just $ FullHouse e a
            | otherwise = Nothing

flush :: [Card] -> Maybe Score
flush hand | sameSuit hand = let (a:b:c:d:e:[]) = sort . map count $ hand
                             in Just $ Flush e d c b a
           | otherwise = Nothing

straight :: [Card] -> Maybe Score
straight hand | isStraight hand = Just . Straight . maximum . map count $ hand
              | otherwise = Nothing

threeOfAKind :: [Card] -> Maybe Score
threeOfAKind = helper . sort . map count
    where helper (a:b:c:d:e:[])
            | a == b && b == c = Just $ ThreeOfAKind a e d
            | b == c && c == d = Just $ ThreeOfAKind b e a
            | c == d && d == e = Just $ ThreeOfAKind c b a
            | otherwise = Nothing

twoPairs :: [Card] -> Maybe Score
twoPairs = helper . sort . map count
    where helper (a:b:c:d:e:[])
            | a == b && c == d = Just $ TwoPairs c b e
            | a == b && d == e = Just $ TwoPairs d b c
            | b == c && d == e = Just $ TwoPairs d c a
            | otherwise = Nothing

onePair :: [Card] -> Maybe Score
onePair = helper . sort . map count
    where helper (a:b:c:d:e:[])
            | a == b = Just $ OnePair a e d c
            | b == c = Just $ OnePair b e d a
            | c == d = Just $ OnePair c e b a
            | d == e = Just $ OnePair d c b a
            | otherwise = Nothing

highCard :: [Card] -> Maybe Score
highCard = helper . sort . map count
    where helper (a:b:c:d:e:[]) = Just $ HighCard e d c b a

scorer :: [[Card] -> Maybe Score]
scorer = [straightFlush, fourOfAKind, fullHouse, flush, straight,
             threeOfAKind, twoPairs, onePair, highCard]

score :: [Card] -> Score
score hand = head . catMaybes . map ($hand) $ scorer

mapTuple = join (***)

fiftyfour = do
    text <- readFile $ "p054_poker.txt"
    let rounds = map (splitAt 5 . map read . words) . lines $ text
    let scores = map (mapTuple score) rounds
    return . length . filter (\(a, b) ->  a > b) $ scores 

-- fiftyfive
revInt :: Integer -> Integer
revInt = foldr (\a b -> 10 * b + a) 0 . map toInteger . dlist

isPalindrome :: Integer -> Bool
isPalindrome x = x == revInt x

palSeq :: Integer -> [Integer]
palSeq x = let next = x + revInt x in (next : palSeq next)

isNotLychrel = any isPalindrome . take 50 . palSeq
fiftyfive n = length . filter (not . isNotLychrel) $ [1..n]

-- fiftysix 
fiftysix = maximum $ do
    a <- [1..99]
    b <- [1..99]
    return . sum . dlist $ a ^ b

-- fiftyseven 
-- TODO(thnd): You should be able to converge an infinite fraction, no?
repFrac :: (Integral a) => [a] -> Ratio a
repFrac [] = undefined
repFrac (x:[]) = fromIntegral x
repFrac (x:xs) = fromIntegral x + 1 / repFrac xs

fiftyseven = length $ do
    n <- [1..1000]
    let fr = repFrac . take n . (1:) . repeat $ 2
    let numDs = dlist . numerator $ fr
    let denDs = dlist . denominator $ fr
    guard $ length numDs > length denDs
    return fr

-- fiftyeight
corners k = do
    d <- [3, 2, 1, 0]
    return $ k^2 - d * (k - 1)

allLayers = [1]: map corners [3, 5..]

fracs :: (Integral a) => (a -> Bool) -> (a, Int, Int) -> [(a, Int, Int)]
fracs p (start, primeCount, totalCount) =
    let deltaP = length . filter p . corners $ start 
    in (start - 2, primeCount, totalCount) : fracs p (start + 2, primeCount + deltaP, totalCount + 4)

slow = fracs isPrime (3, 0, 1)
sparse (a, b, c) = b * 10 < c
fiftyeight = head . filter sparse . fracs isPrime $ (20913, 4293, 41821)
-- checkpoint (8355, 1918, 16705)
-- checkpoint (20913, 4293, 41821)
-- checkpoint (26241, 5248, 52481) *ahem*


-- fiftynine

bytes = [79, 59, 12, 2, 79, 35, 8, 28, 20, 2, 3, 68, 8, 9, 68, 45, 0, 12,
    9, 67, 68, 4, 7, 5, 23, 27, 1, 21, 79, 85, 78, 79, 85, 71, 38, 10, 71, 27, 12,
    2, 79, 6, 2, 8, 13, 9, 1, 13, 9, 8, 68, 19, 7, 1, 71, 56, 11, 21, 11, 68, 6, 3,
    22, 2, 14, 0, 30, 79, 1, 31, 6, 23, 19, 10, 0, 73, 79, 44, 2, 79, 19, 6, 28,
    68, 16, 6, 16, 15, 79, 35, 8, 11, 72, 71, 14, 10, 3, 79, 12, 2, 79, 19, 6, 28,
    68, 32, 0, 0, 73, 79, 86, 71, 39, 1, 71, 24, 5, 20, 79, 13, 9, 79, 16, 15, 10,
    68, 5, 10, 3, 14, 1, 10, 14, 1, 3, 71, 24, 13, 19, 7, 68, 32, 0, 0, 73, 79, 87,
    71, 39, 1, 71, 12, 22, 2, 14, 16, 2, 11, 68, 2, 25, 1, 21, 22, 16, 15, 6, 10,
    0, 79, 16, 15, 10, 22, 2, 79, 13, 20, 65, 68, 41, 0, 16, 15, 6, 10, 0, 79, 1,
    31, 6, 23, 19, 28, 68, 19, 7, 5, 19, 79, 12, 2, 79, 0, 14, 11, 10, 64, 27, 68,
    10, 14, 15, 2, 65, 68, 83, 79, 40, 14, 9, 1, 71, 6, 16, 20, 10, 8, 1, 79, 19,
    6, 28, 68, 14, 1, 68, 15, 6, 9, 75, 79, 5, 9, 11, 68, 19, 7, 13, 20, 79, 8, 14,
    9, 1, 71, 8, 13, 17, 10, 23, 71, 3, 13, 0, 7, 16, 71, 27, 11, 71, 10, 18, 2,
    29, 29, 8, 1, 1, 73, 79, 81, 71, 59, 12, 2, 79, 8, 14, 8, 12, 19, 79, 23, 15,
    6, 10, 2, 28, 68, 19, 7, 22, 8, 26, 3, 15, 79, 16, 15, 10, 68, 3, 14, 22, 12,
    1, 1, 20, 28, 72, 71, 14, 10, 3, 79, 16, 15, 10, 68, 3, 14, 22, 12, 1, 1, 20,
    28, 68, 4, 14, 10, 71, 1, 1, 17, 10, 22, 71, 10, 28, 19, 6, 10, 0, 26, 13, 20,
    7, 68, 14, 27, 74, 71, 89, 68, 32, 0, 0, 71, 28, 1, 9, 27, 68, 45, 0, 12, 9,
    79, 16, 15, 10, 68, 37, 14, 20, 19, 6, 23, 19, 79, 83, 71, 27, 11, 71, 27, 1,
    11, 3, 68, 2, 25, 1, 21, 22, 11, 9, 10, 68, 6, 13, 11, 18, 27, 68, 19, 7, 1,
    71, 3, 13, 0, 7, 16, 71, 28, 11, 71, 27, 12, 6, 27, 68, 2, 25, 1, 21, 22, 11,
    9, 10, 68, 10, 6, 3, 15, 27, 68, 5, 10, 8, 14, 10, 18, 2, 79, 6, 2, 12, 5, 18,
    28, 1, 71, 0, 2, 71, 7, 13, 20, 79, 16, 2, 28, 16, 14, 2, 11, 9, 22, 74, 71,
    87, 68, 45, 0, 12, 9, 79, 12, 14, 2, 23, 2, 3, 2, 71, 24, 5, 20, 79, 10, 8, 27,
    68, 19, 7, 1, 71, 3, 13, 0, 7, 16, 92, 79, 12, 2, 79, 19, 6, 28, 68, 8, 1, 8,
    30, 79, 5, 71, 24, 13, 19, 1, 1, 20, 28, 68, 19, 0, 68, 19, 7, 1, 71, 3, 13, 0,
    7, 16, 73, 79, 93, 71, 59, 12, 2, 79, 11, 9, 10, 68, 16, 7, 11, 71, 6, 23, 71,
    27, 12, 2, 79, 16, 21, 26, 1, 71, 3, 13, 0, 7, 16, 75, 79, 19, 15, 0, 68, 0, 6,
    18, 2, 28, 68, 11, 6, 3, 15, 27, 68, 19, 0, 68, 2, 25, 1, 21, 22, 11, 9, 10,
    72, 71, 24, 5, 20, 79, 3, 8, 6, 10, 0, 79, 16, 8, 79, 7, 8, 2, 1, 71, 6, 10,
    19, 0, 68, 19, 7, 1, 71, 24, 11, 21, 3, 0, 73, 79, 85, 87, 79, 38, 18, 27, 68,
    6, 3, 16, 15, 0, 17, 0, 7, 68, 19, 7, 1, 71, 24, 11, 21, 3, 0, 71, 24, 5, 20,
    79, 9, 6, 11, 1, 71, 27, 12, 21, 0, 17, 0, 7, 68, 15, 6, 9, 75, 79, 16, 15, 10,
    68, 16, 0, 22, 11, 11, 68, 3, 6, 0, 9, 72, 16, 71, 29, 1, 4, 0, 3, 9, 6, 30, 2,
    79, 12, 14, 2, 68, 16, 7, 1, 9, 79, 12, 2, 79, 7, 6, 2, 1, 73, 79, 85, 86, 79,
    33, 17, 10, 10, 71, 6, 10, 71, 7, 13, 20, 79, 11, 16, 1, 68, 11, 14, 10, 3, 79,
    5, 9, 11, 68, 6, 2, 11, 9, 8, 68, 15, 6, 23, 71, 0, 19, 9, 79, 20, 2, 0, 20,
    11, 10, 72, 71, 7, 1, 71, 24, 5, 20, 79, 10, 8, 27, 68, 6, 12, 7, 2, 31, 16, 2,
    11, 74, 71, 94, 86, 71, 45, 17, 19, 79, 16, 8, 79, 5, 11, 3, 68, 16, 7, 11, 71,
    13, 1, 11, 6, 1, 17, 10, 0, 71, 7, 13, 10, 79, 5, 9, 11, 68, 6, 12, 7, 2, 31,
    16, 2, 11, 68, 15, 6, 9, 75, 79, 12, 2, 79, 3, 6, 25, 1, 71, 27, 12, 2, 79, 22,
    14, 8, 12, 19, 79, 16, 8, 79, 6, 2, 12, 11, 10, 10, 68, 4, 7, 13, 11, 11, 22,
    2, 1, 68, 8, 9, 68, 32, 0, 0, 73, 79, 85, 84, 79, 48, 15, 10, 29, 71, 14, 22,
    2, 79, 22, 2, 13, 11, 21, 1, 69, 71, 59, 12, 14, 28, 68, 14, 28, 68, 9, 0, 16,
    71, 14, 68, 23, 7, 29, 20, 6, 7, 6, 3, 68, 5, 6, 22, 19, 7, 68, 21, 10, 23, 18,
    3, 16, 14, 1, 3, 71, 9, 22, 8, 2, 68, 15, 26, 9, 6, 1, 68, 23, 14, 23, 20, 6,
    11, 9, 79, 11, 21, 79, 20, 11, 14, 10, 75, 79, 16, 15, 6, 23, 71, 29, 1, 5, 6,
    22, 19, 7, 68, 4, 0, 9, 2, 28, 68, 1, 29, 11, 10, 79, 35, 8, 11, 74, 86, 91,
    68, 52, 0, 68, 19, 7, 1, 71, 56, 11, 21, 11, 68, 5, 10, 7, 6, 2, 1, 71, 7, 17,
    10, 14, 10, 71, 14, 10, 3, 79, 8, 14, 25, 1, 3, 79, 12, 2, 29, 1, 71, 0, 10,
    71, 10, 5, 21, 27, 12, 71, 14, 9, 8, 1, 3, 71, 26, 23, 73, 79, 44, 2, 79, 19,
    6, 28, 68, 1, 26, 8, 11, 79, 11, 1, 79, 17, 9, 9, 5, 14, 3, 13, 9, 8, 68, 11,
    0, 18, 2, 79, 5, 9, 11, 68, 1, 14, 13, 19, 7, 2, 18, 3, 10, 2, 28, 23, 73, 79,
    37, 9, 11, 68, 16, 10, 68, 15, 14, 18, 2, 79, 23, 2, 10, 10, 71, 7, 13, 20, 79,
    3, 11, 0, 22, 30, 67, 68, 19, 7, 1, 71, 8, 8, 8, 29, 29, 71, 0, 2, 71, 27, 12,
    2, 79, 11, 9, 3, 29, 71, 60, 11, 9, 79, 11, 1, 79, 16, 15, 10, 68, 33, 14, 16,
    15, 10, 22, 73]

genKeys = do
    a <- ['a'..'z']
    b <- ['a'..'z']
    c <- ['a'..'z']
    return . map ord . cycle $ [a, b, c]

theCheck = do
    k <- genKeys
    let output = zipWith xor k bytes
    guard $ isInfixOf (map ord "the") output
    guard $ isInfixOf (map ord "have") output
    return (map chr output, map chr . take 3 $ k)

-- key is "god"
fiftynine = let key = cycle . map ord $ "god"
            in sum . zipWith xor key $ bytes

-- sixty
-- This is very slow!
-- Based on the project thread, the slowest part might be
-- prime verification, or it might be the lack of
-- state being passed forward. Please look into the State Monad.
catNum :: Int -> Int -> Int
catNum a b = read $ show a ++ show b

allP :: (Int -> Bool) -> [Int] -> Bool
allP p ns = and $ do
    n <- ns
    return . and . map (p . catNum n) . delete n $ ns

newP :: (Int -> Bool) -> Int -> [Int] -> Bool
newP p n ns = and $ do
    m <- ns
    return $ (p . catNum m $ n) && (p . catNum n $ m)

pr :: Int -> Bool
pr = isPrime

sixty = do
    let lim = 10000
    let low = primesTil lim
    p1 <- low
    p2 <- takeWhile (< min p1 (lim - p1)) low
    guard $ newP pr p2 [p1]
    p3 <- takeWhile (< min p2 (lim - p1 - p2)) low
    guard $ newP pr p3 [p2, p1]
    p4 <- takeWhile (< min p3 (lim - p1 - p2 - p3)) low
    guard $ newP pr p4 [p3, p2, p1]
    p5 <- takeWhile (< min p4 (lim - p1 - p2 - p3 - p4)) low
    guard $ newP pr p5 [p4, p3, p2, p1]
    return [p1, p2, p3, p4, p5]


-- sixtyone
data Figurate = Tri | Square | Pent | Hex | Hept | Oct deriving (Show, Eq, Ord)

-- genNums :: Figurate -> (Int -> Int) -> [(Int, Figurate)]
-- genNums f p = map (,f) . takeWhile (<10000) . filter (>=1000) . map p $ [1..]

-- tris = genNums Tri (\n -> n * (n + 1) `quot` 2)
-- squares = genNums Square (\n -> n ^ 2)
-- pents = genNums Pent (\n -> n * (3 * n - 1) `quot` 2)
-- hexes = genNums Hex (\n -> n * (2 * n - 1))
-- hepts = genNums Hept (\n -> n * (5 * n - 3) `quot` 2)
-- octs = genNums Oct (\n -> n * (3 * n - 2))

-- allNums = tris ++ squares ++ pents ++ hexes ++ hepts ++ octs

-- chain :: (Integral a) => (a, b) -> (a, b) -> Bool
-- chain (n, _) (m, _) = n `mod` 100 == m `quot` 100

-- findTwo = do
--     n1 <- allNums
--     n2 <- filter (\x -> snd x /= snd n1) allNums
--     guard $ chain n1 n2
--     return (n1, n2)

-- distinctTypes :: (Ord b) => ((a, b), (a, b)) -> ((a, b), (a, b)) -> Bool
-- distinctTypes (n1, n2) (n3, n4) = (== 4) . S.size . S.fromList . map snd $ [n1, n2, n3, n4]

-- findFour = do
--     p1 <- findTwo
--     p2 <- filter (distinctTypes p1) findTwo
--     let (n1, n2) = p1
--     let (n3, n4) = p2
--     if chain n2 n3
--         then return (n1, n2, n3, n4)
--         else do
--             guard $ chain n4 n1
--             return (n3, n4, n1, n2)

-- dt2 :: (Ord b) => ((a, b), (a, b), (a, b), (a, b)) -> ((a, b), (a, b)) -> Bool
-- dt2 (n1, n2, n3, n4) (n5, n6) = (== 6) . S.size . S.fromList . map snd $ [n1, n2, n3, n4, n5, n6]

-- findSix = do
--     p4 <- findFour
--     p2 <- filter (dt2 p4) findTwo
--     let (n1, n2, n3, n4) = p4
--     let (n5, n6) = p2
--     guard $ chain n4 n5 && chain n6 n1
--     return (n1, n2, n3, n4, n5, n6)
   

-- sixtytwo
-- Indexed by a sorted list of digits 
idx :: Integer -> M.Map [Int] [Integer]
idx lim = M.fromListWith (++) . map (\p -> (sort . dlist $ p, [p])) . map (\n -> n^3) $ [1..lim]

atLeastPerms :: Integer -> Int -> M.Map [Int] [Integer]
atLeastPerms lim n = M.filter ((>= n) . length) . idx $ lim


-- sixtythree
-- 10 ^ n has (n + 1) digits, so we need 9^n to have n digits for this
-- to work., i.e. (10/9) ^ n < 10 => n < 21.85
sixtythree = do
    n <- [1..21]
    a <- [1..9]
    guard . (==n) . length . show $ a ^ n
    return (a, n)

-- sixtyfour
data FE = Rad Rational Rational Integer deriving (Eq, Ord)

instance Num FE  where
    (+) (Rad p q x) (Rad r s y) 
        | x == y = Rad (p + r) (q + s) y
        | otherwise = undefined
    negate (Rad p q x) = Rad (-p) (-q) x
    (*) (Rad p q x) (Rad r s y) 
        | x == y = Rad (p * r + fromIntegral x * q * s) (q * r + p * s) x
        | otherwise = undefined
    fromInteger x = undefined
    abs = undefined
    signum = undefined

instance Fractional FE where
    fromRational p = Rad p 0 17
    recip (Rad x y n) = let denom = (x ^ 2 - (fromIntegral n) * y ^ 2)
                        in Rad (x / denom) (- y / denom) n
    (/) p q = p * recip q

instance Real FE where
    toRational (Rad x y n) = x + y * (toRational . sqrt . fromIntegral $ n)

instance RealFrac FE where
    properFraction (Rad x y n) =
        let fl = floor . toRational $ (Rad x y n)
        in (fl, Rad (x - toRational fl) y n)
    floor = fst . properFraction

instance Show FE where
    show p = let (nonper, per) = findCycle . genPairs $ p
                 jc :: [FE] -> String
                 jc = intercalate "," . map (show . floor)
             in "[" ++ jc nonper ++ ";(" ++ jc per ++ ")]"
             
genPairs :: FE -> [FE]
genPairs p = let (b, c) = properFraction p
                 next = recip c
             in p : genPairs next
            
findCycle :: [FE] -> ([FE], [FE])
findCycle = iter []
    where iter seen (p:ps) = let (nonrep, peri) = break (==p) seen
                             in if null peri
                                  -- TODO: avoid append here?
                                  then iter (seen ++ [p]) ps
                                  else (nonrep, peri)

sixtyfour :: Integer -> Int
sixtyfour n = let nonsquare = [1..n] \\ (map square [1..n])
              in length . filter odd . map (length . snd . findCycle . genPairs . Rad 0 1) $ nonsquare

-- sixtyfive
convergents :: ([Integer], [Integer]) -> [Rational]
convergents = undefined

eFrac = (2:) . (1:) . intercalate [1, 1] . map ((:[]) . (2*)) $ [1..]

sixtyfive n = let fr = repFrac . take n $ eFrac
              in sum . dlist . numerator $ fr

-- sixtysix
sols = findCycle . genPairs . Rad 0 1
