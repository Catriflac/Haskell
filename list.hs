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

-- Hoogle magic

replicate' :: Int -> a -> [a]
replicate' n x = replicate n x

and' :: [Bool] -> Bool
and' a = and a

elem' :: Eq a => a -> [a] -> Bool
elem' a b = a `elem` b

maximum' :: Ord a => [a] -> a
maximum' a = maximum a 
