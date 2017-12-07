import Euler
import Data.Ratio
import Control.Monad
import qualified Data.Array as A
import Data.Char
import Data.List
import Data.List.Split
import Debug.Trace
import qualified Data.Map as M
import qualified Data.Ratio as F
import qualified Data.Set as S
import qualified Data.Vector as V

-- This is a bottleneck: for twoseven, 70 seconds is for computing the
-- array and then 7 seconds for the rest of the work!
twosevenPrimes :: A.Array Int Bool
twosevenPrimes = lowNums A.// lowPrimes
    where lowPrimes = zip (primesTil lowMax) (cycle [True])
          lowNums = A.array (1, lowMax) $ zip [1..lowMax] (cycle [False])
          lowMax = 100000

isPrime27 p = twosevenPrimes A.! (abs p)


twoseven = maximum $ do
    a <- [-999, -997 .. 999]
    b <- [-999, -997 .. 999]
    return $ primeGen 0 a b
  where primeGen k a b = if isPrime27 (k ^ 2 + a * k + b)
                            then primeGen (k + 1) a b
                            else (k, a, b)


twoeight = sum $ 1:(map combo [3, 5 .. 1001])
    where combo p = 4 * p ^ 2 - (1 + 2 + 3) * (p - 1) 


bases = [2 .. 100]
bmax = 100
twonine = S.size . S.fromList $ helper bmax bases
helper maxPow = concat . map powersTill
    where powersTill :: Integer -> [Integer]
          powersTill x = iter 2 x []
          iter :: Integer -> Integer -> [Integer] -> [Integer]
          iter n x ys | n > maxPow = ys
          iter n x [] = iter (n + 1) x [x ^ n]
          iter n x (y:ys) = iter (n + 1) x (x * y : y : ys) 

thirty = sum . map read $ do
    num <- [2..200000]
    let str = show num
    guard $ (read str :: Int) == sum (map ((^5) . digitToInt) str)
    return str


numSums :: [Int] -> Int -> Int
numSums _ 0 = 1
numSums [1] n = 1
numSums (x:xs) n = sum . map (numSums xs) $ [n, n - x .. 0]

threeone = numSums [200, 100, 50, 20, 10, 5, 2, 1] 200

thirtyTwo :: Int
thirtyTwo = S.fold (+) 0 . S.fromList $ do
    let naive = filter isPerm (map show [1 .. 10000])
    let nonzero = filter ('0' `notElem`) naive
    x <- nonzero
    y <- filter (isPerm . (x ++)) nonzero
    let z = show $ (read x) * (read y)
    let digits = S.fromList "123456789"
    guard $ (x ++ y ++ z) `isPermOf` digits
    return (read z :: Int)
   
thirtyThree :: Ratio Int 
thirtyThree = product . map (\(a, b) -> a F.% b) $ do
    let digits = ['0' .. '9']
    a <- digits
    b <- digits
    c <- digits
    stop <- [[a, c], [c, a]]
    sbot <- [[b, c], [c, b]]
    let vtop = read stop :: Int
        vbot = read sbot :: Int
    guard $ vtop * (read [b]) == (read [a]) * vbot
    guard $ vtop < vbot
    guard $ last stop /= '0' || last sbot /= '0'
    guard $ head stop /= '0'
    return (vtop, vbot)


factorials :: A.Array Int Int
factorials = A.array (0, 9) ((0, 1) : 
                    [(i, i * factorials A.! (i - 1)) | i <- [1..9]])


tfMax = 400000000

threeFour :: [Int]
threeFour = do
    p <- [3.. tfMax]
    let s = show p
    guard $ p == sum (map ((factorials A.!) . digitToInt) s)
    return p

threeFive :: Int -> Int
threeFive = length . circlePrimes

circlePrimes :: Int -> [Int]
circlePrimes n = filter isCircular $ primesTil n
    where isPrime = (primeArray A.!)
          primeArray = threeFivePrimes n
          isCircular p = and . map isPrime . cycles $ p

cycles :: Int -> [Int] 
cycles n = map (read . take le) . take le . tails . cycle $ str 
    where str = show n
          le = length str
           

threeFivePrimes :: Int -> A.Array Int Bool
threeFivePrimes n = lowNums A.// lowPrimes
    where lowPrimes = zip (primesTil lowMax) (cycle [True])
          lowNums = A.array (1, lowMax) $ zip [1..lowMax] (cycle [False])
          lowMax = n

threesix = sum . filter ispal $ [1..1000000]

bhelp 0 = [0]
bhelp 1 = [1]
bhelp n = (n `mod` 2) : bhelp (n `quot` 2)

binary = reverse . bhelp

binaryPal n = let b = dropWhile (==0) $ binary n
              in  b == reverse b
decimalPal n = show n == reverse (show n)
ispal n = binaryPal n && decimalPal n

threeSevenp = threeFivePrimes 1000000
tfIsPrime = (threeSevenp A.!)

threeSev = take 11 . drop 4 $ truncList

truncList = filter slidable . primesTil $ 1000000
    where slidable p = leftRem p && rightRem p
          leftRem = checkPrime . init . tails . show
          rightRem = checkPrime . tail . inits . show
          checkPrime = and . map (tfIsPrime . read)

pandigital = (`isPermOf` S.fromList "12345679")    

genTris :: Int -> [[Int]]
genTris n = do
    c <- [5, 9 .. n]
    b <- [2, 4 .. c]
    let a = n - b - c
    guard $ a > 0
    guard $ a `mod` 4 == 0
    guard $ a^2 + b^2 == c^2
    return [a, b, c]


pytrips :: [[Int]]
pytrips = do
    m <- [1 .. 500]
    n <- [1 .. m - 1]
    c <- [1 .. 1000 `quot` (2 * m * (m + n))]
    return [m, n, c]

sumTris = map (\[m, n, c] -> (2 * c * m * (m + n))) pytrips

findCounts :: [Int] -> M.Map Int Int
findCounts = foldl iter M.empty 
    where iter mp k = M.insertWith (+) k 1 mp 

threeNine :: Maybe Int
threeNine = elemIndex (maximum ls) ls
    where ls = map (length . genTris) $ [1..1000]

threeNineDumb :: Int -> (Int, Int)
threeNineDumb = maximum . M.toList . findCounts . threeNineDumbHelper

threeNineDumbHelper :: Int -> [Int]
threeNineDumbHelper n = do
    p <- [1..n]
    a <- [1..p - 1]
    b <- [1..a]
    let c = p - a - b
    guard $ a ^ 2 + b ^2 == c^2
    return p
    
    
forty = product . map (digitToInt . (champ !!)) . map (10^) $ [0 .. 6]
    where champ = concat . map show $ [0..]

-- fortyone
-- sum [1..9] `mod` 3 == 0 (not 9)
-- sum [1..8] `mod` 3 == 0 (not 8)
-- sum [1..7] `mod` 7 == 1 (could be 7)
--
ps :: (Eq a) => [a] -> [[a]]
ps [] = [[]]
ps (x:[]) = [[x]]
ps xs = do
    x <- xs
    ts <- ps . filter (/=x) $ xs
    return (x:ts)

lToI :: (Num a) => [a] -> a
lToI [] = 0
lToI (x:xs) = x + 10 * lToI xs

candidates = reverse . sort . map lToI . ps $ [1..7]
fortyOne = head . filter isPrime $ candidates

-- fortytwo
unquote = splitOn "," . filter (/='"')

isTriWord :: String -> Bool
isTriWord wd = elem (decodeTriWord wd) mostTris

decodeTriWord :: String -> Int
decodeTriWord = sum . map (\c -> ord c - ord 'A' + 1)

mostTris = map (\k -> sum [1..k]) [1..100]

fortyTwo = do
    w <- readFile "p042_words.txt"
    return . filter isTriWord . unquote $ w

-- fortyThree
mults n = takeWhile (<1000) . map (n*) $ [1..]

permInt :: Int -> Bool
permInt = isPerm . show


d3 :: Int -> S.Set Int
d3 n
    | n < 100 = S.insert 0 . digits $ n
    | otherwise = digits n

countDigits :: [Int] -> Int
countDigits = S.size . foldr S.union S.empty . map d3

digitMatch :: Int -> Int -> Bool
digitMatch a b = a `mod` 100 == b `quot` 10

compile :: (Num a) => (a, a, a, a) -> a
compile (x, y, z, w) = x * 10^9 + y * 10^6 + z * 10 ^ 3 + w

fortyThree = sum .map compile $ do
    d8910 <- mults 17
    d789 <- mults 13
    guard $ digitMatch d789 d8910
    guard $ (4==) . countDigits $ [d789, d8910]
    d678 <- mults 11
    guard $ digitMatch d678 d789
    guard $ (5==) . countDigits $ [d678, d8910]
    d567 <- mults 7
    guard $ digitMatch d567 d678
    guard $ (6==) . countDigits $ [d567, d8910]
    d456 <- mults 5
    guard $ digitMatch d456 d567 
    guard $ (7==) . countDigits $ [d456, d567, d8910]
    d345 <- mults 3
    guard $ digitMatch d345 d456
    guard $ (8==) . countDigits $ [d345, d567, d8910]
    d234 <- mults 2
    guard $ digitMatch d234 d345
    guard $ (9==) . countDigits $ [d234, d567, d8910]
    let seen = S.union (d3 d234) . S.union (d3 d567) $ d3 d8910
    return (S.elemAt 0 . (S.\\ seen) . S.fromList $ [0..9], d234, d567, d8910)

-- fortyfour
pent n = (n * (3 * n - 1)) `quot` 2
pents :: [Integer]
pents = map pent [1..]

lim :: Int -> [(Integer, Integer, Integer)]
lim n = do
    let p = take n pents
    let s = S.fromList (take (3 * n) pents)
    a <- p
    b <- takeWhile (< a) p
    guard $ S.member (a - b) s
    guard $ S.member (a + b) s
    return (a - b, a, b)
  
-- fortyfive
hex n = n * (2 * n - 1) 
hexes :: [Integer]
hexes = map hex [1..]

tri n = n * (n + 1) `quot` 2
tris :: [Integer]
tris = map tri [1..]

matchTwo :: (Ord a) => [a] -> [a] -> [a]
matchTwo (a:as) (b:bs)
    | a < b = matchTwo as (b:bs)
    | b < a = matchTwo (a:as) bs
    | a == b = a:(matchTwo as bs)
    

matchUp :: (Ord a) => [a] -> [a] -> [a] -> [a]
matchUp as bs cs = matchTwo cs . matchTwo as $ bs

fortyFive = matchUp tris pents hexes
   
-- fortysix 
odds = [1,3..]
twosquares :: [Integer]
twosquares = map (\n -> 2 * n^2) [1..]

pPlusTwoSquare n = S.fromList $ do
    p <- primesTil n
    ts <- takeWhile (<n) twosquares
    let s = p + ts
    guard $ s < n
    return $ s
   
fortySix n = minimum $ do
    let ppts = pPlusTwoSquare n
    let ps = S.fromList . primesTil $ n
    a <- [3,5..n]
    guard $ not . S.member a $ ps
    guard $ not . S.member a $ ppts
    return a 

-- fortyseven
hasFourDistinct = (>=4) . length . filter isPrime . divisors
conseqFourDistinct n = hasFourDistinct n && hasFourDistinct (n + 1) && hasFourDistinct (n + 2) && hasFourDistinct (n + 3)
fortySeven = head . filter conseqFourDistinct $ [1..]

-- fortyeight
newtype ModBig = MakeModBig Integer deriving (Eq, Show)

toMB :: Integer -> ModBig
toMB = MakeModBig . (`mod` 10000000000)

fromMB (MakeModBig x) = x
instance Num ModBig where
    fromInteger = toMB
    x + y = toMB $ (fromMB x) + (fromMB y)
    x - y = toMB $ (fromMB x) - (fromMB y)
    x * y = toMB $ (fromMB x) * (fromMB y)
    abs = undefined
    signum = undefined

fortyEight = fromMB . sum . map (\n -> (toMB n) ^ n) $ [1..1000]

-- fortynine
fortyNine = do
    let primes = filter isPrime [1000..10000]
    let pSet = S.fromList primes
    p0 <- primes
    p1 <- dropWhile (<= p0) primes
    guard $ digits p1 == digits p0
    let p2 = 2 * p1 - p0
    guard $ S.member p2 pSet
    guard $ digits p2 == digits p1
    return (p0, p1, p2)

-- fifty
-- Note: this is only a heuristic that it stops when it does. However, its quite
-- close to a million. Since the starting prime is the 3rd, the sequence length is 543,
-- I think I was grossly overestimating how long the sequence could be.
-- p_100 = 547
-- p_200 = 1229
-- p_300 = 1993
-- p_400 =2749
-- p_500 = 3581
-- To have 78000 primes in a sequence would total 279m
-- Then 600 is a more reasonable maximum: 
--   100 * 547+ 1229 * 100 + 1993 * 100 + 2749 * 100 + 100 * 3581 > 1e6
fifty n = do
    let primes = V.fromList . primesTil $ n
    let pSet = S.fromList . primesTil $ n
    let lp = 600
    sliceLen <- [lp, lp-1..1]
    start <- [0..lp - sliceLen]
    guard $ (primes V.! start) * sliceLen < n
    let s = sum $ V.slice start sliceLen primes
    guard $ s < n
    guard $ S.member s pSet
    return (start, sliceLen, s)
    
