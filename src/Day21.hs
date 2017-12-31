{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Day21
where

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Text
import Text.Megaparsec.Lexer as L
import Data.Text (Text, pack)
import Data.Maybe
import Data.Monoid
import Control.Applicative
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as B
import Data.Map.Strict as M
import Data.List.Split (chunksOf)

type Dim = Int
data GridBoxed a = GridBoxed Dim (B.Vector a) deriving (Show)
data Grid = Grid Dim (U.Vector Int) deriving (Show)

formatGrid :: Grid -> String
formatGrid (Grid n v) = unlines $ formatLine <$> chunksOf n (U.toList v)
    where formatLine xs = (\ i -> if i == 0 then '.' else '#') <$> xs

class Indexable2D a
  where
    ix :: a -> (Int, Int) -> Int
    ixInv :: a -> Int -> (Int, Int)

instance Indexable2D (GridBoxed a)
  where
    ix (GridBoxed dimOuter _) (i, j) = i * dimOuter + j
    ixInv (GridBoxed dimOuter _) n = (n `div` dimOuter, n `mod` dimOuter)

instance Indexable2D Grid
  where
    ix (Grid d _) (i, j) = i * d + j
    ixInv (Grid d _) n = (n `div` d, n `mod` d)

instance Functor GridBoxed
  where fmap f (GridBoxed d v) = GridBoxed d (fmap f v)

type GridHash = (Int, Int)

newtype Permutation4 = P4 (U.Vector Int) deriving Show

instance Monoid Permutation4
  where
    mempty = P4 $ U.fromList [0, 1, 2, 3]
    (P4 v1) `mappend` (P4 v2) = P4 $ U.backpermute v2 v1

newtype Permutation9 = P9 (U.Vector Int) deriving Show

instance Monoid Permutation9
  where
    mempty = P9 $ U.fromList [0, 1, 2, 3, 4, 5, 6, 7, 8]
    (P9 v1) `mappend` (P9 v2) = P9 $ U.backpermute v2 v1

class Permutation a where
  getIdx :: a -> U.Vector Int

instance Permutation Permutation4 where
  getIdx (P4 v) = v

instance Permutation Permutation9 where
  getIdx (P9 v) = v

type Coord = (Int, Int)
  

parseGrid :: Parser Grid
parseGrid = do
  digits <- concat <$> some parseDigit `sepBy` char '/'
  let dim = round . sqrt . fromIntegral . length $ digits
  return $ Grid dim (U.fromList digits)
  where parseDigit = try (char '#' >> return 1) <|> try (char '.' >> return 0)

parseProblem :: Parser [(Grid, Grid)]
parseProblem = parsePair `sepEndBy` eol
  where parsePair =
          do g1 <- parseGrid
             string " => "
             g2 <- parseGrid
             return (g1, g2)


gridHash :: Grid -> GridHash
gridHash (Grid n v) =
  let s = sum $ zipWith (\ i k -> 2^i * k) [0..] (U.toList v)
  in (n, s)


 -- 2 5 8
 -- 1 4 7
 -- 0 3 6
 --

-- 0 1
-- 2 3

-- 1 3
-- 0 2

rot9 = P9 $ U.fromList [2, 5, 8, 1, 4, 7, 0, 3, 6]
flip9 = P9 $ U.fromList [2, 1, 0, 5, 4, 3, 8, 7, 6]

rot4 = P4 $ U.fromList [1, 3, 0, 2]
flip4 = P4 $ U.fromList [1, 0, 3, 2]

makeSymGroup :: Monoid m => m -> m -> [m] 
makeSymGroup rot flip = [mempty, rot, rot <> rot, rot <> rot <> rot,
                         flip, flip <> rot, flip <> rot <> rot, flip <> rot <> rot <> rot]

p4Group = makeSymGroup rot4 flip4
p9Group = makeSymGroup rot9 flip9

act :: Permutation p => p -> Grid -> Grid
act p (Grid dim v) = Grid dim $ U.backpermute v (getIdx p)

actGroup :: Permutation p => [p] -> Grid -> [Grid]
actGroup group x = getZipList $ ZipList (act <$> group) <*> ZipList (repeat x)

createLookup :: [(Grid, Grid)] -> M.Map GridHash Grid
createLookup l = M.fromList $
  do (g1@(Grid n v), g2) <- l
     g1' <- if n == 2 then actGroup p4Group g1
            else if n == 3 then actGroup p9Group g1
            else error "Unexpected grid size"
     return (gridHash g1', g2)

squareIdx :: Int -> Int -> Int -> [Coord]
squareIdx n i j =
  [(i + di, j + dj) | di <- [0..(n-1)], dj <- [0..(n-1)]]

divideIdx :: Int -> Int -> [(Coord, [Coord])]
divideIdx n k =
  let m = n `div` k
  in [((i, j), squareIdx k (i*k) (j*k) ) | i <- [0..(m-1)], j <- [0..(m-1)]]

joinIdx :: Int -> Int -> [(Coord, Coord)]
joinIdx outerDim innerDim =
  let n = outerDim * innerDim
      f k =
        let (i, j)     = (k `div` n, k `mod` n)
            outerCoord = (i `div` innerDim, j `div` innerDim)
            innerCoord = (i `mod` innerDim, j `mod` innerDim)
        in (outerCoord, innerCoord)
  in f <$> [0..(n^2-1)]

divide :: Grid -> Int -> GridBoxed Grid
divide grid@(Grid n v) k =  
  let makeGrid idx = Grid k $ U.backpermute v $ U.fromList idx
      grids = [makeGrid $ ix grid <$> coords | (_, coords) <- divideIdx n k]
  in GridBoxed (n `div` k) $ B.fromList grids

join :: GridBoxed Grid -> Grid
join gb@(GridBoxed outerDim v) =
  let (Grid innerDim _) = v B.! 0
      lookup (outerCoord, innerCoord) =
        let grid@(Grid _ v') = v B.! ix gb outerCoord
        in v' U.! ix grid innerCoord
  in Grid (outerDim * innerDim) $ U.fromList $ lookup <$> joinIdx outerDim innerDim

exampleGrid = Grid 4 (U.fromList [0..15])
foo = divide exampleGrid 2

step :: M.Map GridHash Grid -> Grid -> Grid
step lookup grid@(Grid n v) =
  let k   = if n `mod` 2 == 0 then 2 else 3
      gb  = divide grid k
      gb' = (lookup M.!) . gridHash <$> gb
  in join gb'

startGrid :: Grid
startGrid = Grid 3 $ U.fromList [0, 1, 0, 0, 0, 1, 1, 1, 1]

day21solution1 = do
  contents <- pack <$> readFile "day21_input.txt"
  let pairs      = fromJust $ parseMaybe parseProblem contents
      lookup     = createLookup pairs
      g@(Grid n v) = iterate (step lookup) startGrid !! 18
  print $ U.sum v
