bitsNeeded :: Integer -> Integer
bitsNeeded = (ceiling . logBase 2.0 . fromIntegral)

toBinaryR :: Integer -> Integer -> [Char] -> [Char]
toBinaryR _ (-1) str = str
toBinaryR leftover pow char = if leftover >= 2^pow 
  then (toBinaryR (leftover - 2^pow) (pow - 1) (char ++ "1"))
  else (toBinaryR leftover (pow - 1) (char ++ "0"))

-- Converts an integer like 666
-- into a "binary" string like "1010011010"
toBinary :: Integer -> [Char]
toBinary x = toBinaryR x (bitsNeeded x) ""

matrixOf :: (Integral a) => a -> a -> [(a, a)]
matrixOf x y = [(x, y) | x <- [1..x], y <- [1..y]]

binPairs x y = map (\(a, b) -> (toBinary a, toBinary b)) (mat x y)

zippedPairs x y = map (\(a, b) -> zip a b) (binPairs x y)

matchingCoords x y = filter --fuck
