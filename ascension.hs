-- fiveeightyseven
-- At the intersection point, y =x/n meets (y - 1)^ 2 + (x - 1)^2 = 1.
-- This provides a quadratic equation for x_0
-- The integrals to the left and right are elementary, and were done by hand
x n = ((1 + 1/n) - sqrt (2 / n)) / (1 + 1/n**2)
lineInt n z = z ** 2 / (2 * n)
circInt z = let theta = asin (1 - z) in 1 - z - theta / 2 - sin (2 * theta) / 4
int n = let z = x n in lineInt n z + circInt z
tot = 1 - pi / 4
frac n = int n / tot
nums = zip [1..] . map frac $ [1..]
fiveeightyseven = head . dropWhile ((>0.001) . snd) $nums
