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

digitize :: Int -> [Int]
digitize 0 = []
digitize n = (n `mod` 10) : digitize (n `div` 10)

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
accum s = intercalate "-" $ zipWith (\idx ele -> toUpper ele:replicate (idx-1) (toLower ele)) [1..] s


