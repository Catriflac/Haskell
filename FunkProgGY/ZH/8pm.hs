aWords :: [String] -> [String]
aWords xs = [x | x <- xs, fst x == "a"] 

allToUpper :: String -> String
allToUpper myList = toUpper  myList