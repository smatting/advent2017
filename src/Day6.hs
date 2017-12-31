{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}


module Day6
where

import Control.Monad.ST
import Control.Monad.Loops
import Data.Array.ST
import Data.List
import Data.Maybe
import Data.Ord
import Data.Set as S
import qualified Data.Map as Map


redist :: STUArray s Int Int -> Int -> ST s ()
redist banks i = do
    (_, iLast) <- getBounds banks
    let nexti i = (i + 1) `mod` (iLast + 1)
    let f i' n
          | n > 0 = do
              k <- readArray banks i'
              writeArray banks i' (k+1)
              f (nexti i') (n-1)
          | otherwise = return ()
    n <- readArray banks i
    writeArray banks i 0
    f (nexti i) n

maxBankdIdx xs = snd $ maximumBy (comparing (\(x, i) -> (x, -i))) (zip xs [0..])

day6solution1 :: Int
day6solution1 = runST $ do
    let xs = [10,3,15,10,5,15,5,15,9,2,5,8,5,2,3,6]
    banks <- newListArray (0, length xs - 1) xs :: ST s (STUArray s Int Int)
    let f banksHistory i = do
          xs <- getElems banks
          if xs `member` banksHistory
          then return i
          else do
             redist banks (maxBankdIdx xs)
             f (S.insert xs banksHistory) (i+1)
    f empty 0

day6solution2 :: Int
day6solution2 = runST $ do
    let xs = [10,3,15,10,5,15,5,15,9,2,5,8,5,2,3,6]
    banks <- newListArray (0, length xs - 1) xs :: ST s (STUArray s Int Int)
    let f banksHistory i = do
          xs <- getElems banks
          if xs `Map.member` banksHistory
          then let ii = fromJust (Map.lookup xs banksHistory)
               in return (i - ii)
          else do
               redist banks (maxBankdIdx xs)
               f (Map.insert xs i banksHistory) (i+1)
    f Map.empty 0 
