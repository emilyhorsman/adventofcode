import           Data.List  (find, sort)
import           Data.Maybe (isJust)

type Cmp = String -> String -> Bool


isValid :: Cmp -> [String] -> Bool
isValid _ [] = True
isValid cmp (x : xs) =
    if isJust (find (cmp x) xs) then False else isValid cmp xs


count :: Cmp -> String -> Int
count cmp input =
    length $ filter (isValid cmp) $ map words $ lines input


solve :: Cmp -> FilePath -> IO Int
solve cmp path = count cmp <$> readFile path


isAnagram :: String -> String -> Bool
isAnagram a b =
    sort a == sort b


solve1 = solve (==)
solve2 = solve isAnagram
