xor :: Bool -> Bool -> Bool
xor x y = if x == True && y == False || x == False && y == True
    then True
    else False

doubleEven :: Int -> Int
doubleEven x = if fromIntegral(x `mod` 2) == 0
    then 2 * x
    else 0