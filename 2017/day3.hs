whichRing :: Integral a => a -> a
whichRing n =
    if (ring `mod` 2) == 0 then ring + 1 else ring
        where ring = ceiling (sqrt (fromIntegral n))


distanceFromCorner :: Integral a => a -> a -> a
distanceFromCorner _ 1 = 0
distanceFromCorner n ring =
    let
        start = (ring - 2) ^ 2
        offset = n - start
        arms = (fromIntegral offset) / (fromIntegral (ring - 1))
        upper = start + ceiling arms * (ring - 1)
        diff = upper - n
    in
        min diff (ring - 1 - diff)

steps :: Integral a => a -> a
steps n =
    ring - 1 - (distanceFromCorner n ring)
    where ring = whichRing n
