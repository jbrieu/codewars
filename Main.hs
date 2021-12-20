import Data.Maybe (maybeToList)
import Data.Char ( digitToInt, toUpper, toLower )
import Data.List ( group, intercalate )


main :: IO ()
main = putStrLn "Hello, Haskell!"

evenOrOdd :: Integral a => a -> [Char]
evenOrOdd n
    | even n && (n==0) = "Even"
    | otherwise = "Odd"

positiveSum :: [Int] -> Int
positiveSum = sum . filter (>0)

opposite :: Floating a => a -> a
opposite n = -n

century :: Int -> Int
-- century year = (((year - 1)`div` 100) +1)
century =  (+1) . (`div` 100) . subtract 1



countPositivesSumNegatives :: Maybe [Int] -> [Int]
countPositivesSumNegatives Nothing = []
countPositivesSumNegatives (Just xs) = filter (/=0) [length $ filter (> 0) xs, sum $ filter (< 0) xs]

data Move  = Rock | Paper | Scisors deriving (Eq, Ord, Read, Show, Bounded, Enum)

scores = ["1:0","2:0","3:0","4:0","2:1","3:1","4:1","3:2","4:2","4:3"]

points :: [String] -> Int
points scores = foldr (\(x, y) -> if  x > y then (+3) else if x==y then (+1) else (+0)) 0 pointsPair
        where pointsPair = map (\s -> (digitToInt $ head s, digitToInt $ last s)) scores

rev :: [Char] -> [Char]
rev [] = []
rev (x:xs) = rev xs ++ [x]

doubleChar :: [Char] -> [Char]
doubleChar = (>>= replicate 3)

getMiddle :: String -> String
getMiddle s
        | length s <= 2 = s
        | otherwise = getMiddle $ tail $ take (length s - 1) s

invert :: [Integer] -> [Integer]
invert = map negate

solution :: String -> String -> Int
solution "" _ = 0
solution xs x = length $ filter (==x) $ group xs

testAccum :: IO ()
testAccum = do
        putStrLn $ accum "abcd"

accum :: [Char] -> [Char]
-- accum s = intercalate "-" $ map (\c -> replicate 2 c) s
accum s = intercalate "-" $ zipWith (\idx ele -> Data.Char.toUpper ele:replicate (idx-1) (toLower ele)) [1..] s

-- https://www.codewars.com/kata/57a429e253ba3381850000fb

bmi :: Float -> Float -> String  
bmi weight height
        | bmiCalc <= 18.5 = "Underweight"
        | bmiCalc <= 25.0 = "Normal"
        | bmiCalc <= 30.0 = "Overweight"
        | bmiCalc > 30.0 = "Obese"
        | otherwise = "unknown"
        where bmiCalc = weight/height^2

-- https://www.codewars.com/kata/55f9b48403f6b87a7c0000bd/train/haskell
paperwork :: Int -> Int -> Int
paperwork n m
      | n > 0 && m > 0= n * m
      | otherwise = 0      

-- https://www.codewars.com/kata/55f9bca8ecaa9eac7100004a/train/haskell
past :: Int -> Int -> Int -> Int
past h m s = (h*60*60 + m*60 + s)* 1000 

-- https://www.codewars.com/kata/551b4501ac0447318f0009cd/train/haskell
booleanToString :: Bool -> String
booleanToString True = "True"
booleanToString False = "False"


-- https://www.codewars.com/kata/59ca8246d751df55cc00014c/
hero :: Int -> Int -> Bool
hero bullets dragons = dragons * 2 <= bullets

-- https://www.codewars.com/kata/546e2562b03326a88e000020/train/haskell
-- Apparently input can be negative with haskell version of that kata
squareDigit :: Int -> Int
squareDigit = undigitize . squareDigits . digitize

digitize :: Int -> [Int]
digitize n
  | n <= 9  = [n]
  | otherwise = digitize (n `quot` 10) ++ digitize (n `mod` 10)

undigitize :: [Int] -> Int
undigitize xs = read $ concatMap show xs

squareDigits :: [Int] -> [Int]
squareDigits = map (^2)

-- https://www.codewars.com/kata/57eadb7ecd143f4c9c0000a3/train/haskell
-- based on solution seen afterwards: take 1 is the same as head, but most importantly, map 2 times is a waste, I should have put (toUpper . head) in a single map
getInitials :: String -> String
getInitials s = intercalate "." $ map (take 1) $ words $ map toUpper s