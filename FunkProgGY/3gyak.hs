-- Igazabol mar mindent megtanultatok az elozo ket oran a funkcionalis programozasrol
-- fuggvenyek, fuggvenyalkalmazas, mintaillesztes, tipusok

-- Program vegrehajtasa: egyszerusites (beta-redukciok sorozata)
-- execution  = (+) ((+) 3 2) ((+) 3 4) => ?


------------------------------------------------------------------------
---------------------------- Szorzat tipus -----------------------------
------------------------------------------------------------------------
-- valojaban minden (parameterrel rendelkezo) fuggvenynek 1 parametere van:
-- Curry, uncurry, reszleges fuggvenyalakalmazas
-- peldaul: mod, (+)

-- Oldd meg a masodfoku egyenletet (tfh. van megoldas), add meg mindegyik megoldast
-- Negati

solve :: Int -> Int -> Int -> (Double, Double)
solve a b c = ((-b' + sqrt (b'^2 - 4*a'*c')) / (2*a'), 
               (-b' - sqrt (b'^2 - 4*a'*c')) / (2*a'))
    where
        a' = fromIntegral a
        b' = fromIntegral b
        c' = fromIntegral c

------------------------------------------------------------------------
-------------------------- Mintaillesztés ------------------------------
------------------------------------------------------------------------

-- add meg az is0 fuggvenyt!

is0 :: Int -> Bool
is0 0 = True
is0 _ = False -- Wildcard

is0' :: Int -> Bool
is0' x = case x of
    0 -> True
    _ -> False

is0'' :: Int -> Bool
is0'' x
    | x == 0 = True
    | otherwise = False

is0''' :: Int -> Bool
is0''' x = if x == 0
    then True
    else False

-- add meg a logikai konjunkcio muveletet!
-- if-then-else, case, guardok, mintaillesztes


------------------------------------------------------------------------
--------------------------------- Listák -------------------------------
------------------------------------------------------------------------

-- hany kulonbozo lista van?

-- head

-- tail

-- ures-e?

-- egyelemu-e?

-- Halmazkifejezesek

