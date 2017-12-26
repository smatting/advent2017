module Day2 (day2Solution1, pairs, findFactor, day2Solution2)
where

import Text.Megaparsec
import Text.Megaparsec.String
import Data.Maybe
import Data.List
import Control.Monad

day2Solution1 :: IO ()
day2Solution1 = do
    contents <- readFile "day2_input.txt"
    let problem = fromJust $ parseMaybe problemP contents
    print $ checksum problem


problemP :: Parser [[Int]]
problemP =
    lineP `endBy` eol
    where
        lineP = numberP `sepBy` tab
        numberP = read <$> some digitChar


checksum :: [[Int]] -> Int
checksum problem = sum $
    fmap (\xs -> maximum xs - minimum xs) problem


pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]


findFactor l = head $ do
    (a, b) <- pairs l
    let (a', b') = (max a b, min a b)
    guard $ a' `evenlyDivdedBy` b'
    return $ a' `div` b'
    where
        evenlyDivdedBy a b = a `mod` b == 0


day2Solution2 :: IO ()
day2Solution2 = do
    contents <- readFile "day2_input.txt"
    let problem = fromJust $ parseMaybe problemP contents
    print $ sum $ findFactor <$> problem
