import           Control.Lens
import           Data.List    (elemIndex)
import           Data.Maybe   (isJust)


-- Get the leftmost maximum with its index.
maxWithIndex :: [Int] -> (Int, Int)
maxWithIndex = foldl (\a b -> if snd b > snd a then b else a) (-1, -1) . zip [0..]


redistribute :: Int -> Int -> [Int] -> [Int]
redistribute _ 0 banks = banks
redistribute targetBlock remaining banks =
    redistribute ((targetBlock + 1) `mod` length banks) (remaining - 1) ((element targetBlock +~ 1) banks)


computeCycle :: [Int] -> [Int]
computeCycle banks =
    let
        (index, num) = maxWithIndex banks
        nextBanks = (element index .~ 0) banks
    in
        redistribute ((index + 1) `mod` length banks) num nextBanks


computeCycles :: [[Int]] -> Int -> [Int] -> (Int, Maybe Int)
computeCycles history cycles banks =
    let
        nextBanks = computeCycle banks
        historicalIndex = nextBanks `elemIndex` history
    in
        if isJust historicalIndex then (cycles + 1, (+ 1) <$> historicalIndex) else computeCycles (nextBanks : history) (cycles + 1) nextBanks


parse :: String -> [Int]
parse = map read . words


solve :: FilePath -> IO (Int, Maybe Int)
solve path =
    (computeCycles [] 0 . parse) <$> readFile path
