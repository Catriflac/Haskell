-- ismetles:
inc x = x + 1

x = 0
-- Mi lesz az eredmenye x-nek, ha lefuttatjuk 'inc x'-et?

matches :: (Int, Int) -> (Int, Int) -> Bool
matches (x,y) (n,m) = x==n || x==m || y==n || y==m

-- mintaillesztes es tobb egyenlettel megadott fuggvenyek
-- wildcard minta
-- A felsorolas sorrendje!
-- Add meg a logikai 'es'-t

-- Egy fx meghatározása két részben
not' :: Bool -> Bool
not' True  = False
not' False = True

-- Logikai táblázat
and' :: Bool -> Bool -> Bool
and' True True = True
and' False False = False
and' True False = False
and' False True = False

and'' :: Bool -> Bool -> Bool
and'' True True = True
and'' x y = False    -- Bármi már érték az előre meghatározotton kívül

and''' :: Bool -> Bool -> Bool
and''' True True = True
and''' _ _ = False 

and'''' :: Bool -> Bool -> Bool
and'''' True x = x
and'''' _ _ = False

and''''' :: Bool -> Bool -> Bool
and''''' x y = if x==True
                then if y==True
                    then True
                    else False
                else False


-- Guard -> logikai kifejezés kiértékelése
and6 :: Bool -> Bool -> Bool
and6 x y
    |   x == True = y
    |   otherwise = False


or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

or2 :: Bool -> Bool -> Bool
or2 x y = not' ( and' (not' x) (not' y) )

-- osszeadas kettes szamrendszerben 2 biten:
--add2 :: (Int, Int) -> (Int, Int) -> (Int, Int)
--add2 (x1, x2) (y1, y2) =  

-- Adj meg egy fuggenyt, amely a sortores karaktert szokozre "csereli"
swapEndline :: Char -> Char
swapEndline '\n' = ' '
swapEndline x = x


-- szamologep: calc
calc :: Int -> Char -> Int -> Int
calc x p y = case p of
    '+' -> (x' + y')
    '-' -> (x' - y') 
    '*' -> (x' * y')
    '/' -> (x' / y')
    where
        x' = fromIntegral x
        y' = fromIntegral y


-- osztast is tudo szamologep:
calc' :: Int -> Char -> Int -> Int
calc' = undefined


