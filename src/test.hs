import Data.List
import System.IO

{---- LISTS ----}
primeNumbers = [3,5]
morePrimes = primeNumbers ++ [13, 17, 19]
morePrimes2 = 2 : morePrimes
lenPrime = length morePrimes2
revPrime = reverse morePrimes
is7inList = 7 `elem` morePrimes2
isListEmpty = null morePrimes2
secondPrime = morePrimes2 !! 1
firstPrime = head morePrimes2
lastPrime = tail morePrimes2
primeInit = init morePrimes2
first3Primes = take 3 morePrimes2
removedPrimes = drop 3 morePrimes2
maxPrime = maximum morePrimes2
minPrime = minimum morePrimes2

newList = [2, 3, 5]
productList = product newList
evenList = [2, 4 .. 100]

charList = ['A','C'..'Z']

-- only generates list up to necessary reference index
infiniteList = [10,20..]

many2s = take 10 (repeat 2)
many3s = replicate 10 3

cycleList = take 10 (cycle [1..5])

-- multiplies everything in list by 2
listTimes2 = [x * 2 | x <- [1..10]]

divisBy9N13 = [x | x <- [1..500], {-filter -->  -} x `mod` 13 == 0, x `mod` 9 == 0]

sortedList = sort [9, 1, 5, 4, 6, 8, 10, 100]

sumOfList = zipWith (+) [1..5] [6..10]

listBiggerThan5 = filter (>5) [(-100)..10]

evensUpTo20 = takeWhile (<=20) [2,4..]

--foldl: left-to-right; foldr: right-to-left;
multOfList = foldl (*) 1 [2..10]

-- calculates each value 3^n
pow3List = [3^n | n <- [1..0]]

multTable = [[x * y | y <- [1..10]] | x <- [1..10]]

{---- TUPLES ----}
randTuple = (1, "random tuple")
bobSmith = ("Bob Smith", 52)
bobsName = fst bobSmith
bobsAge = snd bobSmith

{---- FUNCTIONS ----}
getTriple x = x * 3
addMe :: Int -> Int -> Int -- recieves int, int, and outputs int

addMe x y = x + y
sumMe x y = x + y -- can accept any type of input

addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (x, y) (x2, y2) = (x + x2, y + y2)

whatAge :: Int -> String
whatAge 16 = "You can drive"
whatAge 18 = "You can vote"
whatAge 21 = "You can drink"
whatAge x = "Nothing important."

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

prodFact n = product [1..n] -- factorial

isOdd :: Int -> Bool
isOdd n
  | n `mod` 2 == 0 = False
  | otherwise = True

isEven n = n `mod` 2 == 0

whatGrade :: Int -> String
whatGrade age
  | (age >= 5) && (age <= 6) = "Kindergarten"
  | (age >= 13) && (age <= 18) = "High School"
  | otherwise = "Go to college and fuck middle school."

battingAvgRating :: Double -> Double -> String
battingAvgRating hits atBats
  | avg <= 0.200 = "terrible average"
  | avg <= 0.250 = "avg playa"
  | avg <= 0.280 = "above avg playa"
  | otherwise = "you a supastah"
  where avg = hits / atBats

getListItems :: [Int] -> String
getListItems [] = "List be empty"
getListItems (x:[]) = "Your list begins with " ++ show x
getListItems (x:y:[]) = "Your list contains " ++ show x ++ " and " ++ show y
getListItems (x:xs) = "The 1st items is " ++ show x ++ " and the rest are " ++ show xs

{--- HIGHER ORDER FUNCTIONS ---}
times4 :: Int -> Int
times4 x = x * 4

listTImes4 = map times4 [1..5] -- 4, 8, 12, 16, 20

multBy4 :: [Int] -> [Int]
multBy4 [] = []
multBy4 (x:xs) = times4 x : multBy4 xs
-- [1..4] : x = 1 and xs = [2, 3, 4]
-- x = 1, is chopped off and stored in return list
-- then evaluates the remainder list [2,3,4]
-- repeats until no list items leftover

isStrEq :: [Char] -> [Char] -> Bool
isStrEq [] [] = True
isStrEq (x:xs) (y:ys) = x == y && isStrEq xs ys

-- expects arg to be function that accepts Int and returns Int
doMult :: (Int -> Int) -> Int
doMult func = func 3
run3Times4 = doMult times4

getAddFunc :: Int -> (Int -> Int)
getAddFunc x y = x + y
adds3 = getAddFunc 3
fourPlus3 = adds3 4

threePlusLIst = map adds3 [1..5]

doubleEvenNumber y =
  if (y `mod` 2 /= 0)
  then y
  else y * 2

getClass :: Int -> String
getClass n = case n of 
  5 -> "Go to kindergarten"
  6 -> "Go to elementary school"
  _ -> "None other to go"

{---- MODULES ----}
--module SampFunctions (getClass, doubleEvenNumbers, ThreePlusList) where
--import SampFunctions

{---- ENUMERATED TYPE ----}
data BaseballPlayer  = Catcher -- can be on one line
                    | Infielder
                    | Outfield
              deriving Show

barryBonds :: BaseballPlayer -> Bool
barryBonds Outfield = True

barryInOf = print(barryBonds Outfield) -- true

{---- CUSTOM TYPES ----}
data Customer = Customer String String Double
  deriving Show

tomSmith :: Customer
tomSmith = Customer "Tom Smith" "123 address ln" 20.50

getBalance :: Customer -> Double
getBalance (Customer _ _ b) = b -- "getBalance tomSmith" returns 20.50

data RPS = Rock | Paper | Scissors

-- main function must be at top
--shoot :: RPS -> RPS -> String
--shoot Paper Rock == "Paper beats rock"
-- the rest......
--shoot _ _ = "ERRORONEOUS INPUTS YO"

data Shape  = Circle Float Float Float
            | Rectangle Float Float Float Float
    deriving Show

area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2
area (Rectangle x y x2 y2) = (abs $ x2 - x) * (abs $ y2 - y)
-- $ means eval everything after it first

-- '  .  ' chains functions
sumValue = putStrLn (show (1 + 2))
sumValue2 = putStrLn . show $ 1 + 2

areOfCircle = area (Circle 50 60 20)
areOfRect = area $ Rectangle 10 10 100 100

{---- TYPE CLASSES ----}
-- correspond to sets of types which have
-- certain operations defined for them
data Emp = Emp { name :: String,
                 pos :: String,
                 idNum :: Int
                 } deriving (Eq, Show)

-- emp1 == emp2 possible, as well as printing (`show`ing)

data ShirtSize = S | M | L
instance Eq ShirtSize where
  S == S = True
  M == M = True
  L == L = True
  _ == _ = False

instance Show ShirtSize where
  show S = "Small"
  show M = "Medium"
  show L = "Large"

smallAvailible f = f `elem` [S, M, L]

class MyEq a where
  areEqual :: a -> a -> Bool

instance MyEq ShirtSize where
  areEqual S S = True
  areEqual M M = True
  areEqual L L = True
  areEqual _ _ = False

newSize = areEqual M M -- equals True

{---- IO ----}
-- get input
sayHello = do
  putStrLn "What's your name"
  name <- getLine
  putStrLn $ "Hello " ++ name

-- (create if need) and write to file
writeToFile = do
  theFile <- openFile "test.txt" WriteMode
  hPutStrLn theFile ("Random line of text")
  hClose theFile

-- read
readFromFile = do
  theFile2 <- openFile "test.txt" ReadMode
  contents <- hGetContents theFile2
  putStr contents
  hClose theFile2

-- fibonacci
fib = 1 : 1 :[a + b | (a, b) <- zip fib (tail fib)]
-- start by creating list [1,1]
-- (a recieves 1, b recieves 2)
-- adds them together and creates next item, `2` in this case
-- list is now [1,1,2]

main = do
  print $ fib !! 100
