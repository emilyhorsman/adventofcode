import           Data.Vector.Unboxed         (fromList, thaw)
import qualified Data.Vector.Unboxed.Mutable as M


parse :: String -> [Int]
parse = map read . lines


compute fDiff pointer steps instructions = do
    jump <- M.read instructions pointer
    M.modify instructions (+ (fDiff jump)) pointer
    let target = pointer + jump
    let nextSteps = steps + 1
    if target >= M.length instructions || target < 0 then return nextSteps else compute fDiff target nextSteps instructions


solve1 = compute (\_ -> 1) 0 0
solve2 = compute (\jump -> if jump >= 3 then -1 else 1) 0 0


main = do
    instructions <- parse <$> readFile "day5.1.input"
    vector <- thaw $ fromList instructions
    s <- solve1 vector
    print s
    vector' <- thaw $ fromList instructions
    s' <- solve2 vector'
    print s'
