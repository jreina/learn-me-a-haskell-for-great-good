root x = sqrt (fromIntegral x)
magnitude (a, b, c) = root (a^2 + b^2 + c^2)
isInt a = fromIntegral (floor a) == a
hasIntegerMagnitude (a, b, c) = isInt (magnitude (a, b, c))

-- Finds the integral triples between length min and max 
-- that, when treated as a vector, has an integral magnitude.
intVectorsBetween min max = [ (a,b,c) | 
    c <- [min..max], b <- [1..c], a <- [1..b], hasIntegerMagnitude (a, b, c) ]