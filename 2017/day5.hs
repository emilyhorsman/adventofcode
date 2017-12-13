import           Data.Vector.Unboxed         (fromList, thaw)
import qualified Data.Vector.Unboxed.Mutable as M


parse :: String -> [Int]
parse = map read . lines


compute pointer steps instructions = do
    jump <- M.read instructions pointer
    M.modify instructions (+ 1) pointer
    let target = pointer + jump
    let nextSteps = steps + 1
    if target >= M.length instructions || target < 0 then return nextSteps else compute target nextSteps instructions


main = do
    instructions <- parse <$> readFile "day5.1.input"
    vector <- thaw $ fromList instructions
    s <- compute 0 0 vector
    print s
