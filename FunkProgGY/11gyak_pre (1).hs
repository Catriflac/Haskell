import Prelude hiding (Maybe, Either, Just, Nothing, Left, Right)

------------------------------------------------------
-- Algebrai adattipusok folyt., Maybe, Either, Tree --
------------------------------------------------------

-- 3. Tipusparameteres adattipusok
-- 3. a) Maybe

data Maybe a = Nothing | Just a deriving (Eq, Show)

safeDiv :: Int -> Int -> Maybe Int
safeDiv x 0 = Nothing
safeDiv x y = Just $ x `div` y

-- Hogyan lehet Maybe-t hasznalni:
multiplyMaybe :: Maybe Int -> Int -> Maybe Int
multiplyMaybe (Just x) y = Just (x * y)
multiplyMaybe Nothing  _ = Nothing

-- Mi a tipusa a Just Nothing erteknek?


-- Keszitsunk egy map-ot: kulcs-ertek parok listaja. A kulcsok legyenek Intek, ertekek tetszoleges tipusuak.
-- Legyen put, get muvelet (egyszeruseg kedveert csak (==)-t vizsgaljunk, HF: rendezetten megvalositott map)
type Map a = [(Int, a)]

put :: Int -> a -> Map a -> Map a
put k v []     = [(k, v)]
put k v (x:xs) = if fst x /= k then x:put k v xs else (k, v):xs

get :: Int -> Map a -> Maybe a
get _ [] = Nothing
get k ((x, v):xs)
    | k == x  = Just v
    | otherwise = get k xs
-- kulcs párok (x hely v érték)
-- visszaadja az adott kulcshoz tartozó értéket

-- 3. b) Either //két típus uniója  a típus balra, b típus jobbra // tagged union
-- osszeg tipus:  // -> 'Either Char Int'
data Either a b = Left a | Right b deriving (Eq, Show)

-- Adj meg egy Int-eket es String-eket tartalmazo mapot!
exampleMap :: Map (Either Int String)
exampleMap = [(1, Left 1), (2, Right "egystring")]

-- izomorfizmus ujra:
-- Mutasd meg, hogy az Either a () tipus izomorf a Maybe a tipussal // () = unit
f :: Maybe a -> Either a ()
f (Just x) = Left x
f Nothing = Right ()

g :: Either a () -> Maybe a
g = undefined

--------------------------
-- Rekurziv adattipusok --
--------------------------

-- Listak:

data MyList a = Nil | Cons a (MyList a) deriving Show

myHead :: MyList a -> Maybe a
myHead Nil = Nothing
myHead (Cons x xs) = Just x


-- Fa tipus, amely leveleiben es csucsaiban is tartalmaz ertekeket
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show)

--            Leaf   | Node (Tree a) a (Tree a) <- csak a csucsokban tarol erteket
--            Leaf a | Node (Tree a)   (Tree a) <- csak a levelekben tarol erteket


-- data Tree a b = Leaf a | Node (Tree a b) b (Tree a b) <- csucsokban b, levelekben a tipusu ertekeket tarol

myTree = Node (Node (Leaf 1) 3 (Leaf 2)) 7 (Node (Leaf 4) 6 (Leaf 5))
{-
         7
       /   \
      3     6
     / \   / \
    1   2 4   5
-}

-- Add meg a kovetkezo fat:
{-
         10
       /   \
      4     2
     / \ 
    2   3  
-}
myTree2 = undefined

height :: Tree a -> Int
height = undefined

sumTree :: Num a => Tree a -> a
sumTree = undefined

makeList :: Tree a -> [a]
makeList = undefined

-- Megjegyzes: ezeket a fuggvenyeket automatikusan megkapjuk, ha megadjuk a Foldable instance-ot (azaz, pl. a foldr-t)

-- "map"
instance Functor Tree where
  --fmap :: (a -> b) -> Tree a -> Tree b
  --fmap = undefined

-- "foldr"
instance Foldable Tree where
  --foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr = undefined



