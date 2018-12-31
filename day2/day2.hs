import System.IO
import Data.Map
import Data.List


main = do
    contents <- readFile "input"
    let ids = lines contents
    let result = generateChecksum ids 0 0
    print result


charCounts :: String -> [Int]
charCounts word = Prelude.map length $ group $ sort word


hasTwo :: [Int] -> Bool
hasTwo counts = elem 2 counts


hasThree :: [Int] -> Bool
hasThree counts = elem 3 counts


generateChecksum :: [String] -> Int -> Int -> Int
generateChecksum [] twos threes = twos * threes
generateChecksum (x:xs) twos threes
    | hasTwo counts && hasThree counts = generateChecksum xs (twos + 1) (threes + 1)
    | hasTwo counts = generateChecksum xs (twos + 1) threes
    | hasThree counts = generateChecksum xs twos (threes + 1)
    | otherwise = generateChecksum xs twos threes
    where counts = charCounts x
