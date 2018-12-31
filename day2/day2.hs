import System.IO


main = do
    contents <- readFile "input"
    let ids = lines contents
    let result = findCommonLetters ids
    print result


countDifferences :: String -> String -> Int -> Int
countDifferences [] [] count = count
countDifferences (a:as) (b:bs) count
    | a /= b = countDifferences as bs (count + 1)
    | otherwise = countDifferences as bs count


commonLetters :: String -> String -> String -> String
commonLetters (a:as) (b:bs) sameSoFar
    | a == b = commonLetters as bs (sameSoFar ++ [a])
    | a /= b = sameSoFar ++ as


findDifferByOne :: String -> [String] -> String
findDifferByOne base [] = []
findDifferByOne base (x:xs)
    | differByOne = commonLetters base x []
    | otherwise = findDifferByOne base xs
    where differByOne = 1 == countDifferences base x 0


findCommonLetters :: [String] -> String
findCommonLetters [] = "U goofed"
findCommonLetters (x:xs)
    | [] == commonChars = findCommonLetters xs
    | otherwise = commonChars
    where commonChars = findDifferByOne x xs

