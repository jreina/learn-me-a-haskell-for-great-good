
-- bitsNeeded :: Int -> Int
bitsNeeded :: Integer -> Integer
bitsNeeded = (ceiling . logBase 2.0 . fromIntegral)

toBinaryR :: Integer -> Integer -> [Char] -> [Char]
toBinaryR _ (-1) str = str
toBinaryR leftover pow char = if leftover >= 2^pow 
  then (toBinaryR (leftover - 2^pow) (pow - 1) (char ++ "1"))
  else (toBinaryR leftover (pow - 1) (char ++ "0"))

toBinary :: Integer -> [Char]
toBinary x = toBinaryR x (bitsNeeded x) ""
