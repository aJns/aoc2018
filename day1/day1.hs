import System.IO

main = do
  contents <- readFile "input"
  let strList = lines $ stripChars "+" contents
  let intList = map read strList
  let result = sum intList
  print result

stripChars :: String -> String -> String
stripChars = filter . flip notElem
