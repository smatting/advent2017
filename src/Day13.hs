module Day13
where

import Text.Megaparsec
import Text.Megaparsec.Text
import Text.Megaparsec.Lexer as L
import Data.Text (Text, pack)
import Data.Maybe

isCaught depth range =
    let k = depth `mod` (2 * (range - 1))
    in k == 0

severity depth range = depth * range

f depth range =
    if isCaught depth range then severity depth range else 0

problemP :: Parser [(Integer, Integer)]
problemP = pairP `sepEndBy` eol
    where pairP = do
            n <- L.integer
            string ": "
            m <- L.integer
            return (n, m)

isFinallyCaught :: [(Integer, Integer)] -> Bool
isFinallyCaught = any (uncurry isCaught)

delay n = fmap (\ (x, y) -> (x + n, y)) 

day13solution1 = do
    s <- pack <$> readFile "day13_input.txt"
    let l = fromJust $ parseMaybe problemP s
    print $ sum $ fmap (uncurry f) l

delayWorks pairs n = isFinallyCaught (delay n pairs)

day13solution2 = do
    s <- pack <$> readFile "day13_input.txt"
    let l = fromJust $ parseMaybe problemP s
    -- let l =  [(0, 3), (1, 2), (4, 4), (6, 4)]
    print $ head $ filter (not . delayWorks l) [0,1..]
