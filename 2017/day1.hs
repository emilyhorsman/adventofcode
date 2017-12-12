toDigits :: Integral a => a -> [a]
toDigits 0 = []
toDigits n =
    let
        (quotient, remainder) = divMod n 10
    in
        remainder : toDigits quotient


getConsecutives :: Integral a => [a] -> [a] -> [a] -> [a]
getConsecutives whole (x : y : remaining) results
  | x == y = getConsecutives whole (y : remaining) (x : results)
  | otherwise = getConsecutives whole (y : remaining) results
getConsecutives (a : whole) (y : []) results
  | a == y = y : results
getConsecutives _ _ results = results


sumConsecutives :: Integral a => a -> a
sumConsecutives input =
    let
        digits = toDigits input
    in
        sum $ getConsecutives digits digits []


checkConsecutive :: Integral a => [a] -> (a, Int) -> Bool
checkConsecutive list (num, cmpIndex) =
    num == list !! cmpIndex


halfwayConsecutives :: Integral a => a -> a
halfwayConsecutives input =
    let
        digits = toDigits input
        len = length digits
        increment = div len 2
        items = map (\(x, index) -> (x, mod (index + increment) len)) $ zip digits [0..]
    in
        sum $ map fst $ filter (checkConsecutive digits) items
