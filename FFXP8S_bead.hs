{-# LANGUAGE InstanceSigs #-}
import Data.Char
import Control.Arrow
-- import Prelude hiding (Maybe, Either, Just, Nothing, Left, Right)

------------------------------------------------------------ 1
isDecimalDigit :: Char -> Bool
isDecimalDigit vmi =
    case isDigit vmi of
        True    -> True
        False   -> False

------------------------------------------------------------ 1
isInt :: String -> Bool
isInt xs  =
    case dropWhile isDigit xs of
        ""      -> True
        _       -> False

------------------------------------------------------------ 3
scanChar :: Char -> Int
scanChar c | '0' <= c && c <= '9' = fromEnum c - fromEnum '0'
           | otherwise = -1

strToInt :: String -> Int
strToInt = go 0
    where go a [] = a
          go a (x:xs) | 0 <= sc && sc <= 9 = go (10*a+sc) xs
                      | otherwise = 0
              where sc = scanChar x

------------------------------------------------------------ 2
type Stack a = [a]
push :: Stack a -> a -> Stack a
push a xs = [xs] ++ a

pop :: Stack a -> (a, Stack a)
pop (x:xs) = (x, xs)

------------------------------------------------------------ 2
trimStart :: String -> String
trimStart = dropWhile isSpace

breakOnSpace :: String -> (String, String)
breakOnSpace = second trimStart . break isSpace . trimStart

------------------------------------------------------------ 2
type Map a = [(Int, a)]
get :: Eq a => [(a, b)] -> a -> Maybe b
--get :: Int -> Map a -> Maybe a
get [] _ = Nothing
get ((x, v):xs) k
    | k == x  = Just v
    | otherwise = get xs k

------------------------------------------------------------ SEGÉDLET
data Tree a b = Leaf a | Node1 b (Tree a b) | Node2 (Tree a b) b (Tree a b) deriving (Eq,Show)
instance Functor (Tree a) where
  fmap :: (c -> b) -> Tree a c -> Tree a b
  fmap = undefined

type IFun1 = (String, (Int -> Int))
type IFun2 = (String, (Int -> Int -> Int))

funs :: ([IFun1],[IFun2])
funs = ([("abs", abs),("neg", \x -> -x)], [("+", (+)), ("-", (-)), ("*", (*))]) 

------------------------------------------------------------ ERROR
step :: ([IFun1],[IFun2]) ->  Stack (Tree Int String) -> String -> Stack (Tree Int String)
step = undefined

------------------------------------------------------------ ERROR
buildTreeRec :: ([IFun1],[IFun2]) -> Stack (Tree Int String) -> String -> Stack (Tree Int String)
buildTreeRec = undefined

buildTree :: ([IFun1],[IFun2]) -> String -> Tree Int String
buildTree = undefined

------------------------------------------------------------ ERROR
singleFunsInTree :: Tree Int (Either (Int -> Int) (Int -> Int -> Int)) -> [Int -> Int]
singleFunsInTree (Leaf _) = []
singleFunsInTree (Node1 (Left f) t) = f : singleFunsInTree t
singleFunsInTree (Node1 _ t) = singleFunsInTree t
singleFunsInTree (Node2 l _ r) = singleFunsInTree l ++ singleFunsInTree r

applyAllSingle :: [Int -> Int] -> [Int] -> [Int]
applyAllSingle funs xs = map (\(f, x) -> f x) $ zip funs xs

dualFunsInTree :: Tree Int (Either (Int -> Int) (Int -> Int -> Int)) -> [Int -> Int -> Int]
dualFunsInTree (Leaf _) = []
dualFunsInTree (Node1 _ t) = dualFunsInTree t
dualFunsInTree (Node2 l (Right f) r) = dualFunsInTree l ++ [f] ++ dualFunsInTree r
dualFunsInTree (Node2 l _ r) = dualFunsInTree l ++ dualFunsInTree r

applyAllDual :: [Int -> Int -> Int] -> [(Int, Int)] -> [Int]
applyAllDual funs xs = map (\(f, x) -> f x) $ zip (map uncurry funs) xs

tree1 :: Tree Int String
tree1 = Node1 "neg" (Node1 "abs" (Leaf 0))
tree2 :: Tree Int String
tree2 = Node2 (Node2 (Leaf 1) "-" (Leaf 2)) "+" (Node2 (Leaf 3) "*" (Leaf 4))

transform :: ([IFun1],[IFun2]) -> Tree Int String -> Tree Int (Either (Int -> Int) (Int -> Int -> Int))
-- transform funs = fmap _
transform funs = undefined

------------------------------------------------------------ ERROR
evaluateTree :: Tree Int (Either (Int -> Int) (Int -> Int -> Int)) -> Int
evaluateTree = undefined

------------------------------------------------------------ ERROR
{-
Gondoljátok meg, hogy hogyan lehetne egy helyesen zárójelezett kifejezésből (ami egy szöveg) a lengyel formát előállítani (ami szintén egy szöveg)! Egy lehetséges algoritmus elérhető:
https://people.inf.elte.hu/veanna/alg1/segedanyagok/LengyelForma/index.htm
-}