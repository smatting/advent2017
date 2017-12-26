module Day4 where

import Text.Megaparsec
import Text.Megaparsec.String
import Data.Maybe
import Data.List
import Data.Set (fromList)
import Control.Monad
import Linear.V2
import Linear.Vector

foo = fromList ["a", "b", "b"]


isValid xs =
    length xs == length (fromList xs)


problemP :: Parser [[String]]
problemP =
    lineP `endBy` eol
    where
        lineP = some alphaNumChar `sepEndBy` char ' '


day4Solution1 :: IO ()
day4Solution1 = do
    contents <- readFile "day4_input.txt"
    -- parseTest problemP contents
    let passPhrases = fromJust $ parseMaybe problemP contents
    print $ length $ filter isValid passPhrases

isValid2 xs =
    length (fromList ys) == length ys
        where ys = fromList <$> xs


day4Solution2 :: IO ()
day4Solution2 = do
    contents <- readFile "day4_input.txt"
    -- parseTest problemP contents
    let passPhrases = fromJust $ parseMaybe problemP contents
    print $ length $ filter isValid2 passPhrases
