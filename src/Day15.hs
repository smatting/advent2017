module Day15
where

import Data.Bits

step (n1, n2) =
    let x1 = n1 * 16807 `mod` 2147483647
        x2 = n2 * 48271 `mod` 2147483647
    in (x1, x2)

mask :: Integer
mask = shift 2 (16 - 1) - 1

low16 :: Integer -> Integer
low16 n = n .&. mask

match n1 n2 = low16 n1 == low16 n2
    
nums startA startB = iterate step (startA, startB)
example = take 10 $ nums 65 8921

step1 n1 = n1 * 16807 `mod` 2147483647
step2 n2 = n2 * 48271 `mod` 2147483647

gen1 n = filter ((== 0) . (`mod` 4)) (iterate step1 n)
gen2 n = filter ((== 0) . (`mod` 8)) (iterate step2 n)

gen = take 5000000 $ zip (gen1 116) (gen2 299)
genExample = take 5000000 $ zip (gen1 65) (gen2 8921)

day15solution1 = length $ filter id $ fmap (uncurry match) (take 40000000 $ nums 116 299)
day15solution2 = length $ filter id $ fmap (uncurry match) gen
