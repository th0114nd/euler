import Data.Tuple (swap)
import Data.Char
import Data.Array ((!))
import Data.List
import qualified Data.Array as A
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!), (\\))
import qualified Data.Sequence as Sq
import Data.Sequence ((<|), (><), (|>), ViewL(..))
import Control.Monad
import Debug.Trace
import qualified Data.Set as S

--1: sum natural numbers below 1000 divisible by 3 or 5

divCheck :: Int -> Bool
divCheck n = (rem n 5 == 0) || (rem n 3 == 0)

oneishFunc :: Int
oneishFunc = sum . filter divCheck $ [1..1000]


--2: sum even fibonacci numbers whose values do not exceed four million
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

isEven :: Int -> Bool
isEven n = (rem n 2 == 0)

twoishFunc :: Int -> Int
twoishFunc n = n
--twoishFunc n = sum filter isEven takeWhile ((<) n) fibs -- $?


--3: largest prime factor of 600851475143
--sieve

--4: largest palindrome that's the product of two 3-digit numbers
-- diagonal shiz, multiply and check for max

--5: smallest number that is evenly divisible 1 to 20
fiveish = foldl lcm 1 [1..20]
--6 find sumsquares - square sums . [1..100] variance
--7 find the 10,001st prime number
--8 find the greatest product of 5 consecutive numbers in a file
long_num = "4243"

eightish = helper 0 $ map toInt long_num
    where toInt ch = ord ch - ord '0'

helper :: Int -> [Int] -> Int
helper acc (a:b:c:d:e:xs) = helper (max acc (a * b * c * d * e)) (b:c:d:e:xs)
helper acc _ = acc

--9 find a pythagorean triplet for which a + b + c = 1000
ninishHelp :: [(Int, Int, Int)]
ninishHelp = do
    c <- [1..1000]
    b <- [1..max (1000 - c) c]
    a <- [1000 - c - b]
    guard $ a < b
    guard $ a * b * c > 0
    guard (a^2 + b^2 == c^2)
    return (a, b, c)

ninish = head $ map (\(a, b, c)-> a * b *c) ninishHelp


--10 sum of all primes below two million
tenish = sum $ primesTill 2000000

primesTill m = 2 : sieve [3, 5 .. m]
    where
        sieve (p:xs)
            | p * p > m = p : xs
            | otherwise = p : sieve [x | x <- xs, rem x p /= 0]

-- 11: magic

-- 12 triangular numbers
triangular :: Integer -> Integer
triangular n = n * (n + 1) `quot` 2



twelvish = minTri 500

minTri n = head $ dropWhile notLong (map triangular [1..])
    where notLong k = length (divisors k) < n

-- 13: first 10 digits of large sum

nums=[ 37107287533902102798797998220837590246510135740250,
       46376937677490009712648124896970078050417018260538,
       74324986199524741059474233309513058123726617309629,
       91942213363574161572522430563301811072406154908250,
       23067588207539346171171980310421047513778063246676,
       89261670696623633820136378418383684178734361726757,
       28112879812849979408065481931592621691275889832738,
       44274228917432520321923589422876796487670272189318,
       47451445736001306439091167216856844588711603153276,
       70386486105843025439939619828917593665686757934951,
       62176457141856560629502157223196586755079324193331,
       64906352462741904929101432445813822663347944758178,
       92575867718337217661963751590579239728245598838407,
       58203565325359399008402633568948830189458628227828,
       80181199384826282014278194139940567587151170094390,
       35398664372827112653829987240784473053190104293586,
       86515506006295864861532075273371959191420517255829,
       71693888707715466499115593487603532921714970056938,
       54370070576826684624621495650076471787294438377604,
       53282654108756828443191190634694037855217779295145,
       36123272525000296071075082563815656710885258350721,
       45876576172410976447339110607218265236877223636045,
       17423706905851860660448207621209813287860733969412,
       81142660418086830619328460811191061556940512689692,
       51934325451728388641918047049293215058642563049483,
       62467221648435076201727918039944693004732956340691,
       15732444386908125794514089057706229429197107928209,
       55037687525678773091862540744969844508330393682126,
       18336384825330154686196124348767681297534375946515,
       80386287592878490201521685554828717201219257766954,
       78182833757993103614740356856449095527097864797581,
       16726320100436897842553539920931837441497806860984,
       48403098129077791799088218795327364475675590848030,
       87086987551392711854517078544161852424320693150332,
       59959406895756536782107074926966537676326235447210,
       69793950679652694742597709739166693763042633987085,
       41052684708299085211399427365734116182760315001271,
       65378607361501080857009149939512557028198746004375,
       35829035317434717326932123578154982629742552737307,
       94953759765105305946966067683156574377167401875275,
       88902802571733229619176668713819931811048770190271,
       25267680276078003013678680992525463401061632866526,
       36270218540497705585629946580636237993140746255962,
       24074486908231174977792365466257246923322810917141,
       91430288197103288597806669760892938638285025333403,
       34413065578016127815921815005561868836468420090470,
       23053081172816430487623791969842487255036638784583,
       11487696932154902810424020138335124462181441773470,
       63783299490636259666498587618221225225512486764533,
       67720186971698544312419572409913959008952310058822,
       95548255300263520781532296796249481641953868218774,
       76085327132285723110424803456124867697064507995236,
       37774242535411291684276865538926205024910326572967,
       23701913275725675285653248258265463092207058596522,
       29798860272258331913126375147341994889534765745501,
       18495701454879288984856827726077713721403798879715,
       38298203783031473527721580348144513491373226651381,
       34829543829199918180278916522431027392251122869539,
       40957953066405232632538044100059654939159879593635,
       29746152185502371307642255121183693803580388584903,
       41698116222072977186158236678424689157993532961922,
       62467957194401269043877107275048102390895523597457,
       23189706772547915061505504953922979530901129967519,
       86188088225875314529584099251203829009407770775672,
       11306739708304724483816533873502340845647058077308,
       82959174767140363198008187129011875491310547126581,
       97623331044818386269515456334926366572897563400500,
       42846280183517070527831839425882145521227251250327,
       55121603546981200581762165212827652751691296897789,
       32238195734329339946437501907836945765883352399886,
       75506164965184775180738168837861091527357929701337,
       62177842752192623401942399639168044983993173312731,
       32924185707147349566916674687634660915035914677504,
       99518671430235219628894890102423325116913619626622,
       73267460800591547471830798392868535206946944540724,
       76841822524674417161514036427982273348055556214818,
       97142617910342598647204516893989422179826088076852,
       87783646182799346313767754307809363333018982642090,
       10848802521674670883215120185883543223812876952786,
       71329612474782464538636993009049310363619763878039,
       62184073572399794223406235393808339651327408011116,
       66627891981488087797941876876144230030984490851411,
       60661826293682836764744779239180335110989069790714,
       85786944089552990653640447425576083659976645795096,
       66024396409905389607120198219976047599490197230297,
       64913982680032973156037120041377903785566085089252,
       16730939319872750275468906903707539413042652315011,
       94809377245048795150954100921645863754710598436791,
       78639167021187492431995700641917969777599028300699,
       15368713711936614952811305876380278410754449733078,
       40789923115535562561142322423255033685442488917353,
       44889911501440648020369068063960672322193204149535,
       41503128880339536053299340368006977710650566631954,
       81234880673210146739058568557934581403627822703280,
       82616570773948327592232845941706525094512325230608,
       22918802058777319719839450180888072429661980811197,
       77158542502016545090413245809786882778948721859617,
       72107838435069186155435662884062257473692284509516,
       20849603980134001723930671666823555245252804609722,
       53503534226472524250874054075591789781264330331690]

thirteenish :: String
thirteenish = take 10 . show . sum . map read . take 14 . map show $ nums

-- 14: largest collatz sequence under one million.

collatzGen :: Int -> M.Map Int Int
collatzGen max = foldl collatz (M.fromList [(1, 1)]) [1 .. max - 1]
    where collatz :: M.Map Int Int -> Int -> M.Map Int Int
          collatz m k | k `M.member` m = m
                      | otherwise =  M.insert k (1 + m' M.! k') m'
              where k' = if even k
                            then half k
                            else 3 * k + 1
                    m' = collatz m k'
                      
half n = n `quot` 2   

fourteenComp = snd . maximum . map swap . M.toList . collatzGen
fourteenish = fourteenComp 1000000

factorial n = product [1..n]
choose n k = factorial n `quot` ((factorial k) * (factorial (n - k)))
square n = n * n

fifteenish = fifteenComp 20
fifteenComp n = sum . map (square . choose n) $ [0 .. n]

sixteenish :: Int
sixteenish = sum . map digitToInt . show $ 2 ^ 1000

seventeenish = sum . map (length . numToProse . show) $ [1..1000]
checkable = intercalate "\n" $ map (numToProse . show) [1..1000]

numToProse :: String -> String
numToProse "0" = ""
numToProse "1" = "one"
numToProse "2" = "two"
numToProse "3" = "three"
numToProse "4" = "four"
numToProse "5" = "five"
numToProse "6" = "six"
numToProse "7" = "seven"
numToProse "8" = "eight"
numToProse "9" = "nine"
numToProse ('0':[x]) = numToProse [x]
numToProse "10" = "ten"
numToProse "11" = "eleven"
numToProse "12" = "twelve"
numToProse "13" = "thirteen"
numToProse "14" = "fourteen"
numToProse "15" = "fifteen"
numToProse "16" = "sixteen"
numToProse "17" = "seventeen"
numToProse "18" = "eighteen"
numToProse "19" = "nineteen"
numToProse ('2':[x]) = "twenty" ++ numToProse [x]
numToProse ('3':[x]) = "thirty" ++ numToProse [x]
numToProse ('4':[x]) = "forty" ++ numToProse [x]
numToProse ('5':[x]) = "fifty" ++ numToProse [x]
numToProse ('6':[x]) = "sixty" ++ numToProse [x]
numToProse ('7':[x]) = "seventy" ++ numToProse [x]
numToProse ('8':[x]) = "eighty" ++ numToProse [x]
numToProse ('9':[x]) = "ninety" ++ numToProse [x]
numToProse (c:"00") = numToProse [c] ++ "hundred"
numToProse (c:[x, y]) = numToProse [c] ++ "hundredand" ++ numToProse [x, y]
numToProse "1000" = "onethousand"
numToProse x = error $ "not parsed " ++ x

eighteenTri = [
    "75                                           ",
    "95 64                                        ",
    "17 47 82                                     ",
    "18 35 87 10                                  ",
    "20 04 82 47 65                               ",
    "19 01 23 75 03 34                            ",
    "88 02 77 73 07 63 67                         ",
    "99 65 04 28 06 16 70 92                      ",
    "41 41 26 56 83 40 80 70 33                   ",
    "41 48 72 33 47 32 37 16 94 29                ",
    "53 71 44 65 25 43 91 52 97 51 14             ",
    "70 11 33 28 77 73 17 78 39 68 17 57          ",
    "91 71 52 38 17 14 91 43 58 50 27 29 48       ",
    "63 66 04 68 89 53 67 30 73 16 69 87 40 31    ",
    "04 62 98 27 23 09 70 98 73 93 38 53 60 04 23 "
    ]

parsedTri :: [[Int]]
parsedTri = map (map read) $ map (filter (/= "") . splitOn " ") eighteenTri

triParse :: String -> [[Int]]
triParse = filter (/= []) . map (map read) . map (filter (/= "") . splitOn " ") . splitOn "\n"

sumsTo :: (Num a, Ord a) => [[a]] -> a
sumsTo = maximum . foldl combine []

combine :: (Num a, Ord a) => [a] -> [a] -> [a]
combine xs ys = zipWith max (zipWith (+) (0:xs) ys)
                            (zipWith (+) (xs ++ [0]) ys)

eighteenish = sumsTo parsedTri

getTri :: IO [[Int]]
getTri = liftM triParse (readFile "triangle.txt")

sixtysevenish :: IO Int
sixtysevenish = liftM sumsTo getTri


data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Show, Eq, Ord, Enum)

cycleSucc :: Day -> Day
cycleSucc Sunday = Monday
cycleSucc d = succ d


plus :: Day -> Int -> Day
plus d n | n > 7 = plus d (n `mod` 7)
         | n == 0 = d
         | otherwise = plus (cycleSucc d) (n - 1)

data Month = January
            | February
            | March
            | April
            | May
            | June
            | July
            | August
            | September
            | October
            | November
            | December
    deriving (Show, Eq, Ord, Enum)

months = [January .. December]
type Year = Int

daysInMonth :: Year -> Month -> Int
daysInMonth y February = if isLeap y
                            then 29
                            else 28
daysInMonth _ m = if m `elem` [April, June, September, November]
                     then 30
                     else 31

divides :: Int -> Int -> Bool
k `divides` n = n `mod` k == 0

isLeap :: Year -> Bool
isLeap n = (4 `divides` n &&) $ (not (100 `divides` n) || 400 `divides` n) 

daysInYear y = sum . map (daysInMonth y) $ months

firstDay :: Year -> Day
firstDay n = Monday `plus` (sum $ map daysInYear [1900 .. n - 1])

monthStarts :: Year -> [Day]
monthStarts y = map ((firstDay y `plus`) . sum) . inits . map (daysInMonth y) $
        [January .. November]

totalSundays = length . filter (== Sunday) . concat .map monthStarts $ [1901..2000]

twentyish = sum . map digitToInt . show . factorial $ 100

d :: (Integral a) => a -> a
d n = sum (divisors n) - n

divisors' n = (1:) $ nub $ concat [[x, div n x] | x <- [2..limit], rem n x == 0]
        where limit = floor . sqrt . fromIntegral $ n

propDivisors :: (Integral a) => a -> [a]
propDivisors n = 1 : filter ((==0) . rem n) [2 .. n `div` 2] 

divList :: A.Array Int Int
divList = A.listArray (1, 10000) (map d [1..10000])

twentyoneishHelp :: Int -> [Int]
twentyoneishHelp k = do
    b <- [1..k - 1]
    a <- [(max 1 (b - 1000)) .. b - 1]
    guard $ divList A.! a == b
    guard $ divList A.! b == a
    return a
 
twentyoneish = sum $ amic ++ (map d amic)
    where amic = twentyoneishHelp 10000

twotwo :: IO Integer
twotwo = do 
    cxt <- readFile "names.txt"
    let names = parseNames cxt
    return . sum . map fromIntegral . findValue $ names

parseNames :: String -> [String]
parseNames =  sort . read 

findValue :: [String] -> [Int]
findValue ss = zipWith (*) [1..] (map (sum . map charToInt) ss)

charToInt c = ord c - ord 'A' + 1

smallDivList :: A.Array Int Int
smallDivList = A.listArray (1, 28123) (map d [1..28123])

abundants :: [Int]
abundants = map fst . filter (\(a, b) -> a < b) . zip base . map d $ base
        where base = [1..28123]

divisors :: (Integral a) => a -> [a]
divisors n = little ++ (check . reverse) bigs
    where little = filter (\x -> mod n x == 0) [1 .. floor $ sqrt $ fromIntegral n]
          bigs = map (n `quot`) little
          sortaN = fromIntegral n
          check (x:xs) = if x * x == n
                            then xs
                            else (x:xs)

twothree :: Int
twothree = S.fold (+) 0 (S.fromList [1..28123] S.\\ S.fromList abundantSums)
    where abundantSums :: [Int]
          abundantSums = do
            a <- abundants
            b <- takeWhile (<= a) abundants
            return $ a + b 


nthPerm :: (Eq a) => [a] -> Int -> [a]
nthPerm [] _ = []
nthPerm xs n = newHead : nthPerm (delete newHead xs) (n `mod` bucketSize)
    where bucketSize = factorial . (+ (-1)) . length $ xs
          bucketNum = n `quot` bucketSize
          newHead = xs !! bucketNum

twofour = nthPerm "0123456789" 999999

twofiveVal = head . filter ((>= 1000) . length) . map show $ fibs
twofive = elemIndex (read twofiveVal) fibs

twosix = maximum $ zip (map cycleLength [2 .. 999]) [2..999]           
                                                   

cycleLength :: Int -> Int
cycleLength k | k == 1 = 1
              | 2 `divides` k = cycleLength $ k `quot` 2
              | 5 `divides` k = cycleLength $ k `quot` 5
              | otherwise = iter 10 1
    where iter cur count = if 1 == cur
                            then count
                            else iter ((10 * cur) `mod` k) (count + 1)
     
