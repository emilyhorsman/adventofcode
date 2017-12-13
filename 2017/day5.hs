import Control.Lens


parse :: String -> [Int]
parse = (map read) . lines


compute :: Int -> Int -> [Int] -> Int
compute pointer steps instructions =
    let
        target = pointer + instructions !! pointer
        nextInstructions = (element pointer +~ 1) instructions
        nextSteps = steps + 1
     in
        if target >= (length instructions) || target < 0 then nextSteps else compute target nextSteps nextInstructions


--solve :: FilePath -> IO Int
--solve path = (compute 0 0) . (parse <$> readFile path)

main = do
    instructions <- parse <$> readFile "day5.1.input"
    let s = compute 0 0 instructions
    putStrLn (show s)
