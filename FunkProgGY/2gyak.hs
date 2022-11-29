-- TMS: tms.inf.elte.hu
-- Fajl/modul betoltese -> :l(oad)
-- Modul ujratoltes     -> :r(eload)

-- Binaris, hexadecimalis, oktalis literalok
-- :set XBinaryLiterals

-- Definialj egyszeru fuggvenyeket: six, tobb tipussal
-- Ezek közül csak egy definicio kerulhet egy fx-hez:

-- six :: Num p -> p
-- six :: Int
-- six :: Double

six = 6


-- Definiald a double fuggvenyt!

double :: Int -> Int
double x = x * 2

-- Definiald az even fuggvenyt! Lehetseges-e? Meg tudod hivni?
-- even utkozik a beepitett even Prelude fx-el, modulnevvel elotte hivhato: Main.even 2 == True

isEven :: Int -> Bool
isEven x = mod x 2 == 0

-- logikai fuggvenyek
-- Megszerkesztheto-e egy haromszog a harom oldala alapjan?

isTriangle :: Double -> Double -> Double -> Bool
isTriangle x y z = x + y > z && x + z > y && y + z > x

-- lustasag:
alma :: Int -> Bool
alma x = even x || 0 == (fromIntegral x / 0)
-- fromIntegral: egesz törtként való értelmezés
-- vagy div

-- Pitagoraszi szamharmas-e a 3 megadott parameter? HF

-- Szorzat tipus: rendezett n-es (~~~~ rekordok)
-- Adj ossze ket racionalis szamot! Racionális szám (p,q), ahol p = szamlalo, q = nevezo
addRac :: (Int, Int) -> (Int, Int) -> (Int, Int)
-- addRac x y = (fst x * snd y + fst y * snd x, snd x * snd y)
-- mintaillesztes
addRac (x1,x2) (y1,y2) = (x1 * y2 + y1 * x2, x2 * y2)

-- Szorozz ossze ket racionalis szamot!

-- Adott egy helyvektor. Nyujtsd meg konstanszorosara!

-- Szamtipusok kozotti "konverzio"
-- Kerekitesek: round, floor, ceiling, truncate
-- Szamold ki x y-nal vett maradekanak gyoket!

-- Oldd meg az egyutthatoival megadott masodfoku egyenletet!

