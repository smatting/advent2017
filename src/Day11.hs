{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Day11
where

import Text.Megaparsec
import Text.Megaparsec.Text
import Data.Text (Text, pack)
import qualified Data.Map.Strict as M
import Data.Foldable as F
import Data.Maybe
import qualified Data.Vector as V
import Control.Monad.State

type CubeCoord = (Int, Int, Int)
type HexCoord = (Int, Int)

hexToCube :: HexCoord -> CubeCoord
hexToCube (q, r) = (q, r, -q-r)

cubeDist :: CubeCoord -> CubeCoord -> Int
cubeDist (q1, r1, s1) (q2, r2, s2) =
    maximum [d q1 q2, d r1 r2, d s1 s2]
    where d x y = abs $ x - y

hexDist :: HexCoord -> HexCoord -> Int
hexDist hex1 hex2 =
    let cube1 = hexToCube hex1
        cube2 = hexToCube hex2
    in cubeDist cube1 cube2

foo = hexDist (0, 0) (-3, -1)

directionP :: Parser HexCoord
directionP = 
        f "ne" (-1, 1)
    <|> f "se" (0, 1)
    <|> f "nw" (0, -1)
    <|> f "sw" (1, -1)
    <|> f "n" (-1, 0)
    <|> f "s" (1, 0)
    where f s coords = string s >> return coords 

dirsP :: Parser [HexCoord]
dirsP = do
    dirs <- directionP `sepBy` char ','
    eol
    return dirs

hexPlus :: HexCoord -> HexCoord -> HexCoord
hexPlus (q1, r1) (q2, r2) =
    (q1 + q2, r1 + r2)

day11solution1 = do
    s <- pack <$> readFile "day11_input.txt"
    let coords = fromJust $ parseMaybe dirsP s
        dest = F.foldl' hexPlus (0, 0) coords
    print $ hexDist (0, 0) dest

day11solution2 = do
    s <- pack <$> readFile "day11_input.txt"
    let coords = fromJust $ parseMaybe dirsP s
        dists = hexDist (0, 0) <$> scanl hexPlus (0, 0) coords
    print $ maximum dists
