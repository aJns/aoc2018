import System.IO

main = do
  contents <- readFile "input"
  let strList = lines $ stripChars "+" contents
  let intList = map read strList
  let result = findRepeating [0] intList
  print result

stripChars :: String -> String -> String
stripChars = filter . flip notElem

findRepeating :: [Int] -> [Int] -> Int
findRepeating sumList intList
  | elem curr sumList = curr
  | otherwise = findRepeating (curr:sumList) (tail intList)
  where curr = (head sumList) + (head intList)
