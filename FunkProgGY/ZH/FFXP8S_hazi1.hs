modDiv :: Integer -> Integer -> (Integer, Integer)
modDiv x y = (mod x y, div x y)

len :: (Double, Double) -> Double
len (x, y) = sqrt(x*x+y*y)

stretch :: Num p => (p, p) -> p -> (p, p)
stretch (x, y) z = (x*z , y*z)

distance2 :: (Double, Double) -> (Double, Double) -> Double
distance2 (x,y) (n,m) = sqrt((x-n)*(x-n)+(y-m)*(y-m))

distance :: (Int, Int) -> (Int, Int) -> Double
distance (x,y) (n,m) = sqrt(fromIntegral((x-n)^2+(y-m)^2))

matches :: (Integer, Integer) -> (Integer, Integer) -> Bool
matches (x,y) (n,m) = x==n || x==m || y==n || y==m