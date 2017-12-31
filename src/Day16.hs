{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Day16
where

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Text
import Text.Megaparsec.Lexer as L
import Data.Text (Text, pack)
import Control.Monad.ST
import Data.Foldable as F
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as V

data DanceMove = Spin Int | Exchange Int Int | Partner Char Char
  deriving (Show)

parseMoves =  do
  x <- parseMove `sepEndBy` char ','
  eol
  return x

parseMove :: Parser DanceMove
parseMove =
  do {char 's'; n <- fromIntegral <$> L.integer; return (Spin n)}
  <|>
  do char 'x'
     n1 <- fromIntegral <$> L.integer
     char '/'
     n2 <- fromIntegral <$> L.integer
     return (Exchange n1 n2)
  <|>
  do char 'p';
     a <- alphaNumChar
     char '/'
     b <- alphaNumChar
     return (Partner a b)

dance vec (Exchange n1 n2) =
  let a = vec V.! n1
      b = vec V.! n2
  in vec V.// [(n1, b), (n2, a)]

dance vec (Spin k) =
  let n   = V.length vec
      idx = V.fromList [(x - k) `mod` n | x <- [0..n-1]]
  in V.backpermute vec idx

dance vec (Partner a b) =
  V.map f vec
  where
    f c
      | c == a = b
      | c == b = a
      | otherwise = c

getPermutation :: V.Vector Char -> V.Vector Int
getPermutation = V.map (enum M.!)
  where enum = M.fromList $ zip ['a'..'p'] [0..] 

combine p1 p2 =
  V.backpermute p2 p1

repeatPerm p n = F.foldl' combine p (replicate (n-1) p)

getDance = do 
  contents <- pack <$> readFile "day16_input.txt"
  let moves = fromJust $ parseMaybe parseMoves contents
      vec   = V.fromList ['a'..'p']
  return $ F.foldl' dance vec moves

p = V.fromList [7,9,8,6,13,11,4,2,0,12,1,15,5,3,14,10] :: V.Vector Int

cycles :: V.Vector Int -> [[Int]]
cycles p = cycles' p (V.toList p)
  where
    cycles' p [] = []
    cycles' p (x:xs) =
      let c   = getCycle p x
          xs' = filter (not . (`elem` c)) xs
      in c : cycles' p xs'

    followCycle p x = x : followCycle p (p V.! x)

    getCycle p x = x : takeWhile (/=x) (tail $ followCycle p x)

order :: V.Vector Int -> Int
order = foldl' lcm 1 . fmap length . cycles

applyPerm :: V.Vector Int -> (Int -> a) -> [a]
applyPerm perm f = f <$> V.toList perm

day16solution1 = do
  result <- getDance
  print result
