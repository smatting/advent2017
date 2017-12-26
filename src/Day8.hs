{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Day8
where

import Prelude hiding (foldl')
import Text.Megaparsec
import Text.Megaparsec.Text
import Text.Megaparsec.Lexer as L
import Data.Text (pack, Text)
import Data.Foldable as F (foldl', foldr, asum)
import Data.Maybe
import qualified Data.Map.Strict as M

data Inst = Inst Register Op Cond deriving (Show)
type Register = Text
data Op = Inc Int | Dec Int deriving (Show)
data Cond = Cond Register Rel Int deriving (Show)
data Rel = R_LT | R_LEQ | R_EQ | R_GEQ | R_GT | R_NEQ deriving (Show)

type RegisterState = M.Map Register Int

signedIntegerP = L.signed (return ()) integer

opP :: Parser Op
opP = do
    ops <- string "inc" <|> string "dec"
    char ' '
    i <- fromIntegral <$> signedIntegerP
    return $ case ops of "inc" -> Inc i
                         "dec" -> Dec i

condP :: Parser Cond
condP =
    asum (makeParser <$> map)
    where
        makeParser (s, rel) = try $ do
            string "if "
            register <- pack <$> some alphaNumChar
            char ' '
            string s
            char ' '
            i <- fromIntegral <$> signedIntegerP
            return $ Cond register rel i
        map = [("<", R_LT),
               ("<=", R_LEQ),
               ("==", R_EQ),
               (">=", R_GEQ),
               (">", R_GT),
               ("!=", R_NEQ)]

instP :: Parser Inst
instP = do
    register <- pack <$> some alphaNumChar
    char ' '
    op <- opP
    char ' '
    cond <- condP
    return $ Inst register op cond

programP :: Parser [Inst]
programP = instP `sepEndBy` eol

allRegisters :: [Inst] -> [Register]
allRegisters insts = do
    Inst reg1 _ (Cond reg2 _ _) <- insts
    [reg1, reg2]

initRegisters :: [Register] -> RegisterState
initRegisters registers = M.fromList $ zip registers (repeat 0)

getOp :: Rel -> (Int -> Int -> Bool)
getOp R_LT = (<)
getOp R_LEQ = (<=)
getOp R_EQ = (==)
getOp R_GEQ = (>=)
getOp R_GT = (>)
getOp R_NEQ = (/=)

testCondition :: Cond -> RegisterState -> Bool
testCondition (Cond register rel n) state =
    fromMaybe False $ getOp rel <$> M.lookup register state <*> pure n

runInst :: RegisterState -> Inst -> RegisterState
runInst state (Inst reg op cond) =
    if testCondition cond state
        then
            let opFn n = case op of Inc d -> n + d
                                    Dec d -> n - d
            in M.adjust opFn reg state
        else state

runInstR :: Inst -> RegisterState -> RegisterState
runInstR (Inst reg op cond) state =
    if testCondition cond state
        then
            let opFn n = case op of Inc d -> n + d
                                    Dec d -> n - d
            in M.adjust opFn reg state
        else state

example = "\
\b inc 5 if a > 1\n\
\a inc 1 if b < 5\n\
\c dec -10 if a >= 1\n\
\c inc -20 if c == 10\n"

day8solution1 = do
    s <- pack <$> readFile "day8_input.txt"
    let insts = fromMaybe [] (parseMaybe programP s)
        state = initRegisters (allRegisters insts)
        finalState = foldl' runInst state insts
    print $ maximum (M.elems finalState)

day8solution2 = do
    s <- pack <$> readFile "day8_input.txt"
    let insts = fromMaybe [] (parseMaybe programP s)
        state = initRegisters (allRegisters insts)
        states = scanl runInst state insts
    print $ maximum (maximum <$> states)
