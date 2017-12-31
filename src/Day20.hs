{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Day20
where

import Text.Megaparsec
import Text.Megaparsec.Text
import Text.Megaparsec.Lexer as L
import Data.Text (Text, pack)
import Data.Maybe
import Data.Ord
import qualified Data.Vector.Unboxed as V
import Data.Foldable as F

import Data.Map.Strict as M


type Vec3 = V.Vector Int

vplus = V.zipWith (+) 

tick :: Vec3 -> (Vec3, Vec3) -> (Vec3, Vec3)
tick acc (pos, vel) =
  let vel' = vel `vplus` acc
      pos' = pos `vplus` vel'
  in (pos', vel')

norm :: Vec3 -> Int
norm v = V.sum $ V.map abs v

dists :: Vec3 -> Vec3 -> Vec3 -> [Int]
dists acc pos vel = 
  (norm . fst) <$> iterate (tick acc) (pos, vel)

mindist :: Vec3 -> Vec3 -> Vec3 -> Int -> Int
mindist acc pos vel n =  
  last $ take n $ dists acc pos vel

foo = mindist (V.fromList [-4, 1, 1]) (V.fromList [-833,-499,-1391]) (V.fromList [84,17,61]) 10

parseVec3 :: Parser Vec3
parseVec3 = 
  between (char '<') (char '>') $ do
    l <- (fromIntegral <$> L.signed (return ()) L.integer) `sepBy` char ','
    return $ V.fromList l

parseLine :: Parser (Vec3, Vec3, Vec3)
parseLine = do
  string "p="
  p <- parseVec3
  string ", "
  string "v="
  v <- parseVec3
  string ", "
  string "a="
  a <- parseVec3
  return (p, v, a)

parseChallenge :: Parser [(Vec3, Vec3, Vec3)]
parseChallenge = do
  xs <- parseLine `sepEndBy` eol
  eof
  return xs

idxminBy :: Ord a => (b -> a) -> [b] -> Int
idxminBy f xs = 
  fst $ minimumBy (comparing (f . snd)) (zip [0..] xs)

day20solution1 = do
  contents <- pack <$> readFile "day20_input.txt"
  let xs = fromJust $ parseMaybe parseChallenge contents
  print $ idxminBy (\ (p, v, a) -> mindist a p v 2000) xs

groupByKey' assocs = M.fromListWith (++) [(k, [v]) | (k, v) <- assocs]
groupBy' f xs = groupByKey' $ fmap (\ x -> (f x, x)) xs

step :: [(Int, Vec3, Vec3, Vec3)] -> [(Int, Vec3, Vec3, Vec3)]
step xs =
  let xs'     = fmap f' xs
      xsByPos = groupBy' (\ (i, p, v, a) -> p) xs'
  in concat $ M.elems $ M.filter ((== 1) . length) xsByPos
  where
    f' (i, p, v, a) = let (p', v') = tick a (p, v) in (i, p', v', a)

day20solution2 = do
  contents <- pack <$> readFile "day20_input.txt"
  let xs  = fromJust $ parseMaybe parseChallenge contents
      xs' = (\ (i, (p, v, a)) -> (i, p, v, a) ) <$> zip [0..] xs
  print $ length xs'
  print $ length $ iterate step xs' !! 1000
