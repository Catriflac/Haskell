longest :: Int -> Int -> Double
longest a b = sqrt(fromIntegral(a^2) + fromIntegral(b^2))

matches :: (Int, Int) -> (Int, Int) -> Bool
matches (x,y) (n,m) = x==n || x==m || y==n || y==m