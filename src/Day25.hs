{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Day25
where

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Text
import Text.Megaparsec.Lexer as L
import Data.Text (Text, pack)
import Data.Maybe
import Data.Ord
import Data.List
import Data.Map.Lazy as Map
import Data.Sequence as Seq

data State = A | B | C | D | E | F deriving (Show, Eq, Read, Ord)
data Value = Zero | One deriving (Show, Eq)
data Direction = DirLeft | DirRight deriving (Show, Eq)
data Inst = Inst Value Direction State deriving (Show, Eq)
data Rule = Rule State Inst Inst deriving (Show, Eq)

type Position = Int
data Tape = Tape (Seq.Seq Value) Position deriving (Show)
data TMState = TMState State Tape deriving (Show)

type Program = Map State Rule

lineP = many $ satisfy (/= '\n')
padded parser = many (char ' ') >> parser

valueP :: Parser Value
valueP = (string "0" >> return Zero) <|> (string "1" >> return One)

enum parser = do
    padded (string "- ")
    p <- parser
    char '.'
    eol
    return p

stateP :: Parser State
stateP = read . (:[]) <$> alphaNumChar

directionP :: Parser Direction
directionP = (string "left" >> return DirLeft) <|> (string "right" >> return DirRight)

instP :: Parser Inst
instP = do
    lineP
    eol
    writeValue <- enum $ do {string "Write the value "; valueP}
    dir <- enum $ do {string "Move one slot to the "; directionP}
    nextState <- enum $ do {string "Continue with state "; stateP}
    return $ Inst writeValue dir nextState

stateProgramP = do
    string "In state "
    whenState <- stateP
    char ':'
    eol
    inst0 <- instP
    inst1 <- instP
    return $ Rule whenState inst0 inst1

programP :: Parser Program
programP = do
    lineP >> eol
    lineP >> eol
    lineP >> eol
    l <- stateProgramP `sepBy` eol
    return $ Map.fromList $ do {rule@(Rule x _ _) <- l; return (x, rule)}

move :: Tape -> Direction -> Tape
move (Tape seq pos) dir =
    let pos' = pos + dirToNum dir
    in next seq pos' 
    where dirToNum dir =
            case dir of DirLeft -> -1
                        DirRight -> 1
          next seq pos 
              | pos >= Seq.length seq  = Tape (seq |> Zero) pos
              | pos < 0                = Tape (Zero <| seq) 0
              | otherwise              = Tape seq pos

write :: Tape -> Value -> Tape
write (Tape seq pos) value =
    let seq' = Seq.update pos value seq
    in Tape seq' pos

currentVal :: Tape -> Value
currentVal (Tape seq pos) = seq `Seq.index` pos

runStep :: Program -> TMState -> TMState
runStep program (TMState state tape) =
    let (Rule _ inst0 inst1) = fromJust $ state `Map.lookup` program
        Inst writeValue dir nextState = getInst tape inst0 inst1
        tape' = write tape writeValue
        tape'' = move tape' dir
    in seq tape'' (TMState nextState tape'')
    where getInst tape inst0 inst1 =
            case currentVal tape of Zero -> inst0
                                    One -> inst1

initState = TMState A (Tape (Seq.singleton Zero) 0)

iterN :: (a -> a) -> a -> Int -> a
iterN f x n =
    if n == 0 then x
    else 
        let x' = f x
            n' = n - 1
        in seq x' (iterN f x' n')

day25solution1 = do
    contents <- pack <$> readFile "day25_input.txt"
    let program = fromJust $ parseMaybe programP contents
        tms@(TMState state (Tape s pos)) = iterN (runStep program) initState 12794428
    print $ Seq.length $ Seq.filter (== One) s
