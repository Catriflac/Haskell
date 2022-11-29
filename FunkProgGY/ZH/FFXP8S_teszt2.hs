mult :: Integral p => (p, p) -> (p, p) -> (p, p)
mult (a,b) (c,d) = (a*c, b*d)

pairDiv :: (Double, Double) -> Double -> (Double, Double)
pairDiv (a,b) c = (a/c, b/c)