isSpace :: Char -> Bool
isSpace x = if x == ' '
    then True
    else False

not' :: Bool -> Bool
not' x = if x == False
    then True
    else False

or' :: Bool -> Bool -> Bool
or' x y = if x == True || y == True
    then True
    else False

xor :: Bool -> Bool -> Bool
xor x y = if x == True && y == False || x == False && y == True
    then True
    else False

precalc :: Integer -> Char -> Integer -> Integer
precalc x p y = case p of
    '+' -> x + y
    '-' -> x - y 
    '*' -> x * y

calc :: Integer -> Char -> Integer -> Double
calc x p y = case p of
    '+' -> (x' + y')
    '-' -> (x' - y') 
    '*' -> (x' * y')
    '/' -> (x' / y')
    where
        x' = fromIntegral x
        y' = fromIntegral y