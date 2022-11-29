-- importok:
import Data.Char --karakterekkel kapcsolatos fx-ek
import Data.List --listákkal (azonos típusú elemeket taralmazó tömb) kapcsolatos fx-ek
import Distribution.SPDX (LicenseId(AFL_3_0))

-----------------------------------------------------------------

-- lista kifejezesek, literalok


-- listak tipusa, elemek tipusa, stringek
-- [[]] :: [[a]]
-- [1,2,3] :: Num p => [p], de lehetne [Int]
-- [True, [False]] --típushiba
-- [[], [[]]] :: üreslista és egy üreslista-lista :: [[[a]]]
-- [[(True, 'a')], [(True, 'a'), (False, 'b')]] :: [[(Bool, Char)]]
-- ([], []) :: ([a], [b])


-- beepitett fuggvenyek hasznalata listakra //a lista tartalma változatlan marad!

-- : //elejére tűz értéket, !! //indexen lévő elemet adja vissza, ++, reverse, nub //duplikátum kiszedő, sort //sorrendbe rakó, take //első n elemet adja vissza, drop //első n elemet elhagyja, length //hossz

-- Hoogle tipusokkal


-- hany fajta listat ismersz?
-- üres [] vagy nem üres (x:xs) //x az első elem, xs a maradék

headPart :: [a] -> a --csak egy elem lesz
headPart [] = error "No 1st element"
headPart (x:xs) = x

tailPart :: [a] -> [a] --tail listát listára képez!
tailPart [] = error "No tail element(s)"
tailPart (x:xs) = xs


-- ures-e egy lista?
isEmpty1 :: [a] -> Bool
isEmpty1 l = length l == 0

isEmpty2 :: [a] -> Bool
isEmpty2 [] = True
isEmpty2 _ = False

-- egyelemu-e egy lista?
isSingletonList :: [a] -> Bool
isSingletonList [] = False
isSingletonList (x:[]) = True
isSingletonList _ = False 

isSingletonList2 :: [a] -> Bool
isSingletonList2 (x:xs) = case xs of 
                            [] -> True
                            _ -> False

-- ketelemu-e egy lista?

containsTwo :: [a] -> Bool
containsTwo [_,_] = True
containsTwo _ = False

-- elemmódosítás


-- stb. Hogyan lehetne megirni azt, hogy 139 elemu-e egy lista?

containsN :: [a] -> Int -> Bool
containsN xs n = length xs == n

containsN2 :: [a] -> Int -> Bool
containsN2 [] n = n == 0
containsN2 (x:xs) n = containsN xs (n-1)


-- rekurzio: nezzuk meg egyesevel kibontva a beta-redukciokat!
-- szorozd ossze a lista elemeit!


mult :: Num p => [p] -> p
mult [] = 1   --egységelem
mult (x:xs) = x * mult xs 


myand :: [Bool] -> Bool
myand [] = True     --egységelem
myand (x:xs) = x && myand xs 


-- Adj meg egy listat, amely -10-tol 10-ig tartalmazza az egesz szamokat!
-- : a listaképzés

-- szorozd meg egy lista paros elemeit kettovel!

double :: [Int] -> [Int]
double [] = []
double (x:xs) -- = (if even x then x*2 else x) : double xs
  | even x = 2*x : double xs
  | odd x = x : double xs

--

asd :: [a] -> Int -> Int -> [a]
asd xs m n = take (n - m + 1) (drop m xs)

-- Hany darab paratlan eleme van egy listanak?


-- Add meg egy lista n-edik elemet! (error fuggveny)


-- Add meg az osszefesulo rendezes osszefesulo lepeset (merge fuggveny)!
-- Ebben a lepesben ket listat kell rendezetten osszefesulni.


-- adatbazisok: listak + n-esek kombinacioja
-- Az alabbi adatbazis egy hasznaltauto-kereskedes adatbazisa.
-- Minden markahoz fel vannak sorolva az elerheto tipusok, uzemanyag tipussal, es egy szammal
-- amely azt mondja meg, hogy hany darab van keszleten
database =
  [ ("KIA", [("Optima", "plug-in", 5), ("Soul", "electric", 2), ("CEE'D", "plug-in", 2), ("CEE'D", "gasoline", 12), ("XCEE'D", "plug-in", 3)])
  , ("Renault", [("Zoe", "electric", 4), ("Megane", "plug-in", 2), ("Megane", "gasoline", 5), ("Captur", "plug-in", 1)])
  , ("Nissan", [("Leaf", "electric", 6), ("Qashqai", "mild-hybrid", 1), ("X-Trail", "gasoline", 2)])
  , ("Opel", [("Corsa", "gasoline", 2), ("Corsa", "electric", 3)])
  , ("Toyota", [("RAV4", "hybrid", 4), ("Camry", "hybrid", 2), ("Corolla", "hybrid", 9), ("Prius", "plug-in", 3)])
  , ("Ssangyong", [("Korando", "diesel", 4), ("Rexton", "diesel", 2)])
  , ("Mazda", [("MX-30", "electric", 1), ("3", "mild-hybrid", 2), ("6", "gasoline", 1)])
  ]
-- mi a fenti adatbazis tipusa?

-- Komplex feladat: Add meg, hogy hany darab elektromos autot arulnak a kereskedesben!


