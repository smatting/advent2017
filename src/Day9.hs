{-# LANGUAGE OverloadedStrings #-}

module Day9
where

import Prelude
import Text.Megaparsec
import Text.Megaparsec.Text
import Text.Megaparsec.Lexer as L
import Data.Text as T (pack, Text, length)
import Data.Foldable as F (foldl', foldr, asum)
import Control.Applicative as App (empty)
import Data.Maybe
import Data.Char

newtype Group = Group [GroupChild] deriving (Show)
newtype Garbage = Garbage Text deriving (Show)
data GroupChild = ChildGroup Group | ChildGarbage Garbage deriving (Show)

groupChildP :: Parser GroupChild
groupChildP = try g1 <|> try g2
    where g1 = do {group <- groupP; return $ ChildGroup group}
          g2 = do {garbage <- garbageP; return $ ChildGarbage garbage }

isEasyGarbageChar :: Char -> Bool
isEasyGarbageChar c =
    isPrint c && c /= '>' && c /= '!'

easyGarbageChar = satisfy isEasyGarbageChar

garbageP :: Parser Garbage
garbageP = do
    text <- (pack . concat) <$> between (char '<') (char '>') (many garbageContentP)
    return $ Garbage text
    where garbageContentP =
                    try ((:[]) <$> easyGarbageChar)
                <|> try (char '!' >> printChar >> return "")
    
groupP :: Parser Group
groupP = do
    children <- between (char '{') (char '}') (groupChildP `sepBy` char ',')
    return $ Group children

score :: Group -> Int
score = score' 1
    where score' n (Group children) = n + sum (f n <$> children)
          f n (ChildGarbage garbage) = 0
          f n (ChildGroup group) = score' (n + 1) group

countGarbage :: Group -> Int
countGarbage (Group children) = sum (f <$> children)
    where
        f (ChildGarbage (Garbage text)) = T.length text
        f (ChildGroup group) = countGarbage group

day9solution1 = do
    s <- pack <$> readFile "day9_input.txt"
    let result = parse groupP "string" s
    case result of
        Left err -> print err
        Right g -> print $ score g

day9solution2 = do
    s <- pack <$> readFile "day9_input.txt"
    let result = parse groupP "string" s
    case result of
        Left err -> print err
        Right g -> print $ countGarbage g

day9test = do
    let s = ""
    parseTest garbageP s
