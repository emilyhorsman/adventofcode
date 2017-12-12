parse :: String -> [[Integer]]
parse input =
    map ((map read) . words) $ lines input


checksum :: [[Integer]] -> Integer
checksum input =
    sum $ map (\a -> (maximum a) - (minimum a)) input


solve :: FilePath -> IO Integer
solve path = (checksum . parse) <$> readFile path


pairs :: [Integer] -> [(Integer, Integer)]
pairs (x : []) = []
pairs (x:xs) = (map ((,) x) xs) ++ pairs xs


max' = uncurry max
min' = uncurry min


pairsChecksum :: [(Integer, Integer)] -> Integer
pairsChecksum [] = 0
pairsChecksum (pair : xs) =
    let
        (quotient, remainder) = divMod (max' pair) (min' pair)
    in
        if remainder == 0 then quotient else pairsChecksum xs


rowChecksum :: [Integer] -> Integer
rowChecksum = pairsChecksum . pairs


checksum' :: [[Integer]] -> Integer
checksum' input = sum $ map rowChecksum input


solve' :: FilePath -> IO Integer
solve' path = (checksum' . parse) <$> readFile path
