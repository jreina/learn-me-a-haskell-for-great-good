getIndices :: Int -> [Int]
getIndices n = reverse [0..n - 1]

vectorize :: [Char] -> [(Char, Int)]
vectorize str = zip str (getIndices (length str))

getBitVals :: [Char] -> [Integer]
getBitVals str = map (\(a, b) -> if a == '0' then 0 else 2^b) (vectorize str)

-- Converts a "binary" string like "1010011010" 
-- into an integer like 666
fromBinary :: [Char] -> Integer
fromBinary str = sum (getBitVals str)
