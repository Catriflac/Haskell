import Data.Char
import Data.List

database =
  [ ("KIA", [("Optima", "plug-in", 5), ("Soul", "electric", 2), ("CEE'D", "plug-in", 2), ("CEE'D", "gasoline", 12), ("XCEE'D", "plug-in", 3)])
  , ("Renault", [("Zoe", "electric", 4), ("Megane", "plug-in", 2), ("Megane", "gasoline", 5), ("Captur", "plug-in", 1)])
  , ("Nissan", [("Leaf", "electric", 6), ("Qashqai", "mild-hybrid", 1), ("X-Trail", "gasoline", 2)])
  , ("Opel", [("Corsa", "gasoline", 2), ("Corsa", "electric", 3)])
  , ("Toyota", [("RAV4", "hybrid", 4), ("Camry", "hybrid", 2), ("Corolla", "hybrid", 9), ("Prius", "plug-in", 3)])
  , ("Ssangyong", [("Korando", "diesel", 4), ("Rexton", "diesel", 2)])
  , ("Mazda", [("MX-30", "electric", 1), ("3", "mild-hybrid", 2), ("6", "gasoline", 1)])
  ]

-- greenBrands :: [(String, [(String, String, Integer)])] -> Int
-- adatbázis bejárása (párokból áll a db (brand, cars) és a carst tovább definiáljuk (typ, fuel, count) majd szűrjük)
-- végül a nub kiszűri a duplimátumokat a length pedig visszaadja az eredmény hosszát // $ olyan, mint egy zárójel csak egyszerűbb
greenBrands xs = length $ nub [brand | (brand, cars) <- xs, (typ, fuel, count) <- cars, fuel == "electric" || fuel == "plug-in"]

-- $ operator pontositas:
-- csak fuggvenyek alkalmazasakor lehet hasznalni
-- :t ($) :: (a -> b) -> a -> b
-- megadunk neki egy fuggvenyt (f :: a -> b) es egy erteket (x :: a), es
-- visszaadja (f x)-et, viszont nem kell x-et bezarojelezni

---------------------------
-- AGGREGACIO/HAJTOGATAS --
---------------------------

-- Add meg egy lista összegét!
summ :: Num a => [a] -> a
summ []     = 0
summ (x:xs) = x + summ xs

-- Add meg egy lista paros elemeinek osszeget!
sumEvens :: [Int] -> Int
sumEvens [] = 0
sumEvens (x:xs) = sumEvens xs + if even x
                                then x
                                else 0

-- Alternativ javaslat filterrel? - fuggvenykompozicio
sumEvens2 = summ . filter even

-- Valogass szet egy listat az elemek paritasa szerint!
oddEven :: [Int] -> ([Int], [Int])
oddEven [] = ([], [])
oddEven (x:xs) = case oddEven xs of
                    (ys, zs) -> if even x
                                then (x:ys, zs)
                                else (ys, x:zs)
-- bal oldal ys párosok, jobb oldal zs páratlanok
-- összefűzés

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr f d [] = d
myfoldr f d (x:xs) = f x (myfoldr f d xs)

summ' :: Num a => [a] -> a
summ' = foldr (+) 0

sumEvens' :: [Int] -> Int
sumEvens' = foldr (\x acc -> acc + if even x
                                then x
                                else 0) 0

oddEven' :: [Int] -> ([Int], [Int])
oddEven' = foldr (\x acc -> case acc of
                    (ys, zs) -> if even x
                                then (x:ys, zs)
                                else (ys, x:zs)) ([], [])

-- \x a fejelem, acc a rekurzív hívás eredménye

electrics' xs = undefined

-- foldr1

-- tail recursion, foldl', rekurzio egyeb tipusokon (pl szamok)
myfoldl = undefined

-- $ operatoros modon felirt fact:
fact :: Integer -> Integer
fact 0 = 1
fact n = (n *) $ fact $ n - 1

fact2 :: Integer -> Integer -> Integer
fact2 = undefined

fib :: Integer -> Integer
fib = undefined

fib2 :: Integer -> Integer -> Integer -> Integer
fib2 = undefined


