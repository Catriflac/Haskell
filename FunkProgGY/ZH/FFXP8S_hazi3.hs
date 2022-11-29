isSpace1 :: Char -> Bool
isSpace1 x = if x == ' '
    then True
    else False

isSpace2 :: Char -> Bool
isSpace2 x = case x of
    ' ' -> True
    _ -> False

isSpace3 :: Char -> Bool
isSpace3 x
    |   x == ' ' = True
    |   otherwise = False

not1 :: Bool -> Bool
not1 x = if x == False
    then True
    else False

not2 :: Bool -> Bool
not2 x 
    |   x == False = True
    |   otherwise = False   

not3 :: Bool -> Bool
not3 x = case x of
    True -> False
    False -> True 

or1 :: Bool -> Bool -> Bool
or1 x y = if x == True || y == True
    then True
    else False

or2 :: Bool -> Bool -> Bool
or2 x y 
    |   x == True = True
    |   y == True = True

xor1 :: Bool -> Bool -> Bool
xor1 x y = if x == True && y == False || x == False && y == True
    then True
    else False

xor2 :: Bool -> Bool -> Bool
xor2 x y
    |   x == True && y == False = True
    |   x == False && y == True = True
    |   otherwise = False

precalc1 :: Integer -> Char -> Integer -> Integer
precalc1 x p y = case p of
    '+' -> x + y
    '-' -> x - y 
    '*' -> x * y

precalc2 :: Integer -> Char -> Integer -> Integer
precalc2 x p y 
    |   p == '+' = x + y
    |   p == '-' = x - y 
    |   p == '*' = x * y

calc1 :: Integer -> Char -> Integer -> Double
calc1 x p y = case p of
    '+' -> (x' + y')
    '-' -> (x' - y') 
    '*' -> (x' * y')
    '/' -> (x' / y')
    where
        x' = fromIntegral x
        y' = fromIntegral y

calc2 :: Integer -> Char -> Integer -> Double
calc2 x p y
    |   p == '+' = (x' + y')
    |   p == '-' = (x' - y') 
    |   p == '*' = (x' * y')
    |   p == '/' = (x' / y')
    where
        x' = fromIntegral x
        y' = fromIntegral y