isValid :: [String] -> Bool
isValid [] = True
isValid (x : xs) =
    if elem x xs then False else isValid xs


count :: String -> Int
count input =
    length $ filter isValid $ map words $ lines input


solve :: FilePath -> IO Int
solve path = count <$> readFile path
