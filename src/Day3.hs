module Day3 where

import Text.Megaparsec
import Text.Megaparsec.String
import Data.Maybe
import Data.List
import Control.Monad
import Linear.V2
import Linear.Vector

ringIndex :: Int -> Int
ringIndex n = floor $ (1 + sqrt y) / 2
    where y = fromIntegral (n - 1)

startNumber :: Int -> Int
startNumber ringIndex = 2 + 4 * ringIndex * (ringIndex - 1)

divs :: Int -> Int -> [Int]
divs a b = 
    replicate n b ++ [k]
    where n = a `div` b
          k = a `mod` b

posOfStartNumber :: Int -> V2 Int
posOfStartNumber ringIndex = V2 ringIndex (1 - ringIndex)

sideLength :: Int -> Int
sideLength ringIndex = 2 * ringIndex

pos :: Int -> V2 Int
pos n = p0 ^+^ pd
    where
        ri = ringIndex n
        p0 = posOfStartNumber ri ^+^ V2 0 (-1)
        d = n - startNumber ri + 1
        ds = divs d (sideLength ri)
        pd = sumV $ zipWith (*^) ds [V2 0 1, V2 (-1) 0, V2 0 (-1), V2 1 0]


manhattanOrigin :: V2 Int -> Int
manhattanOrigin (V2 x y) = abs x + abs y

day3solution1 = manhattanOrigin (pos 368078) 
