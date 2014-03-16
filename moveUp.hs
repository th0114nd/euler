import Euler
import Data.Ratio
import Control.Monad
import qualified Data.Array as A
import Data.Char
import Data.List
import qualified Data.Map as M
import qualified Data.Ratio as F
import qualified Data.Set as S

-- This is a bottleneck: for twoseven, 70 seconds is for computing the
-- array and then 7 seconds for the rest of the work!
twosevenPrimes :: A.Array Int Bool
twosevenPrimes = lowNums A.// lowPrimes
    where lowPrimes = zip (primesTill lowMax) (cycle [True])
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
circlePrimes n = filter isCircular $ primesTill n
    where isPrime = (primeArray A.!)
          primeArray = threeFivePrimes n
          isCircular p = and . map isPrime . cycles $ p

cycles :: Int -> [Int] 
cycles n = map (read . take le) . take le . tails . cycle $ str 
    where str = show n
          le = length str
           

threeFivePrimes :: Int -> A.Array Int Bool
threeFivePrimes n = lowNums A.// lowPrimes
    where lowPrimes = zip (primesTill lowMax) (cycle [True])
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

truncList = filter slidable . primesTill $ 1000000
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
findCounts = foldr iter M.empty 
    where iter mp k = M.insertWith (+) k 1 mp 

threeNine :: Maybe Int
threeNine = elemIndex (maximum ls) ls
    where ls = map (length . genTris) $ [1..1000]

forty = product . map (digitToInt . (champ !!)) . map (10^) $ [0 .. 6]
    where champ = concat . map show $ [0..]
