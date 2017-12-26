module Day10
where

import qualified Data.Vector.Unboxed as V
import Data.Char
import Data.List.Split (chunksOf)
import Numeric (showIntAtBase, showHex)
import Data.Bits (xor)
import Data.String
import qualified Data.Foldable as F

clamp :: Int -> Int -> Int
clamp n i =
    min (max 0 i) n

twist :: (V.Unbox a) => Int -> Int -> V.Vector a -> V.Vector a
twist i len l =
    let l2 = l V.++ l
        n = V.length l

        -- before revslice
        aLen = clamp n i
        a = V.slice 0 aLen l

        -- revslice
        b = V.slice i len l2
        br = V.reverse b

        -- after revslice
        cLen = clamp (2 * n) $ (2 * n) - aLen - len
        c = V.slice (aLen + len) cLen l2

        l3 = V.concat [a, br, c]

        k = max 0 (n - aLen)
        d1 = V.slice n aLen l3
        d2 = V.slice aLen k l3

    in V.concat [d1, d2]

twist' :: (V.Unbox a) => Int -> Int -> [a] -> V.Vector a
twist' i len l = twist i len (V.fromList l)

hashTwist :: V.Vector Int -> [Int] -> V.Vector Int
hashTwist = hashTwist' 0 0
    where hashTwist' i skipSize x [] = x
          hashTwist' i skipSize x (len:lengths) =
              let x' = twist i len x
                  i' = (i + len + skipSize) `mod` V.length x
                  skipSize' = skipSize + 1
              in hashTwist' i' skipSize' x' lengths

day10solution1 =
    let lengths = [129,154,49,198,200,133,97,254,41,6,2,1,255,0,191,108]
        x = V.fromList [0..255]
        xfinal = hashTwist x lengths
    in (xfinal V.! 0) * (xfinal V.! 1)


knotHashNum :: [Int] -> [Int]
knotHashNum xs =
    let lengths = xs ++ [17, 31, 73, 47, 23]
        x = V.fromList [0..255]
        xfinal = hashTwist x $ take (64 * length lengths) (cycle lengths)
        xored = F.foldl' xor 0  <$> chunksOf 16 (V.toList xfinal)
    in xored


day10solution2 = do
    s <- head . lines <$> readFile "day10_input.txt"
    let xored = knotHashNum (fmap ord s)
        hash = concat $ formatByteHex <$> xored
    putStrLn hash

formatByteHex :: Int -> String
formatByteHex n =
    let s = showHex n ""
        pad = replicate (2 - length s) '0'
    in pad ++ s
