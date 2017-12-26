{-# LANGUAGE FlexibleContexts #-}

module Day5
where

import Control.Monad.ST
import Control.Monad.Loops
import Data.Array.ST
import Data.Maybe
import Data.STRef
import Data.Array.IArray
import Data.Array.MArray (freeze, getBounds)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
 
inBounds :: Int -> (Int, Int) -> Bool
inBounds i (imin, imax) = (imin <= i) && (i <= imax)
 

runSolution1 :: [Int] -> (Int -> Int) -> ST s [()]
runSolution1 xs rule = do
    arr <- newListArray (0, length xs - 1) xs :: ST s (STUArray s Int Int)
    jref <- newSTRef 0
    whileM 
        (inBounds <$> readSTRef jref <*> getBounds arr) $
        do
            j <- readSTRef jref
            arr'j <- readArray arr j
            writeSTRef jref (j + arr'j)
            writeArray arr j (rule arr'j)
            return ()

-- runSolution1' :: [Int] -> ST s Int
-- runSolution1' xs = do
--     let size = (0, (length xs) - 1)
--     arr <- newListArray size xs :: ST s (STUArray s Int Int)
--     let step j i
--           | inBounds j size = do
--               arr'j <- readArray arr j
--               writeArray arr j (arr'j + 1)
--               step (j + arr'j) (i + 1)
--           | otherwise = return i
--     step 0 0


problemP :: Parser [Int]
problemP = (fromIntegral <$> L.signed (return ())  L.integer) `sepEndBy` eol


day5solution1 = do
    s <- readFile "day5_input.txt"
    let xs = fromJust $ parseMaybe problemP s
    let n = length $ runST $ runSolution1 xs (+1)
    print n


day5solution2 = do
    s <- readFile "day5_input.txt"
    let xs = fromJust $ parseMaybe problemP s
    -- let xs = [0, 3, 0, 1, -3]
    let n = length $ runST $ runSolution1 xs (\x -> x + (if x >= 3 then -1 else 1))
    print n

    
-- day5solution1 = length $ runST $ runSolution1 [0, 3, 0, 1, -3]
