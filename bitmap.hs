getIndices :: Int -> [Int]
getIndices n = reverse [0..n - 1]

vectorize :: [Char] -> [(Char, Int)]
vectorize str = zip str (getIndices (length str))

getBitVals :: [Char] -> [Integer]
getBitVals str = map (\(a, b) -> if a == '0' then 0 else 2^b) (vectorize str)

getBigAssNum :: [Char] -> Integer
getBigAssNum str = sum (getBitVals str)
