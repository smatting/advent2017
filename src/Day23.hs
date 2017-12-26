{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Day23
where

import Text.Megaparsec
import Text.Megaparsec.Text
import Text.Megaparsec.Lexer  as L hiding (space)
import Data.Text (Text, pack)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Vector as V
import Control.Monad.State

type Register = Char
type ProgramCounter = Int

data ValRef = Reference Register | Literal Int deriving (Show)

class Resolvable r where 
    resolve :: r -> RegisterState -> Int

data OpType = Set | Sub | Mul deriving (Show, Eq)

data Op = Op {opType:: OpType, register:: Register, valRef:: ValRef} deriving (Show)
data Jump = Jump {testValRef :: ValRef, offsetValRef :: ValRef} deriving (Show)
data Inst = InstOp Op | InstJump Jump deriving (Show)

type RegisterState = M.Map Register Int
data PState = PState RegisterState ProgramCounter deriving (Show)

data MyState = MyState PState Int Int deriving (Show)

instance Resolvable ValRef where
    resolve (Literal n) regs = n
    resolve (Reference reg) regs = fromJust $ M.lookup reg regs

instance Resolvable Register where
    resolve reg regs = fromJust $ M.lookup reg regs

signedIntP = fromIntegral <$> L.signed (return ()) integer

valRefP :: Parser ValRef
valRefP = try f1 <|> try f2
    where f1 = do {n <- signedIntP; return $ Literal n}
          f2 = do {reg <- alphaNumChar; return $ Reference reg}

instP :: Parser Inst
instP = makeOpP "set" Set
    <|> makeOpP "sub" Sub
    <|> makeOpP "mul" Mul
    <|> jumpP
    where makeOpP s opType = try $
            do
                string s
                space
                register <- alphaNumChar
                space
                valRef <- valRefP
                return $ InstOp Op {opType=opType, register=register, valRef=valRef}
          jumpP = try $
            do
                string "jnz"
                space
                testValRef <- valRefP
                space
                offsetValRef <- valRefP
                return $ InstJump Jump {testValRef=testValRef, offsetValRef=offsetValRef}

programP :: Parser [Inst]
programP = instP `sepEndBy` eol

applyRegs :: Inst -> RegisterState -> RegisterState
applyRegs (InstJump _) regs = regs
applyRegs (InstOp op) regs =
    let v = resolve (valRef op) regs
    in M.adjust (flip (fn op) v) (register op) regs
    where fn op = case opType op of
              Set -> \ r v -> v
              Sub -> (-)
              Mul -> (*)

applyPC :: Inst -> RegisterState -> Int -> Int
applyPC (InstOp _) regs i = i + 1
applyPC (InstJump jump) regs i =
      let testVal = resolve (testValRef jump) regs
          offset = resolve (offsetValRef jump) regs
      in if testVal /= 0 then i + offset else i + 1

applyState :: PState -> Inst -> PState
applyState (PState regs i) inst = PState (applyRegs inst regs) (applyPC inst regs i)

runStep :: V.Vector Inst -> PState -> Maybe (PState, Inst)
runStep insts state@(PState regs i) = do
      inst <- insts V.!? i
      return (applyState state inst, inst)

emptyRegisters = M.fromList $ zip "abcdefgh" [0,0..]
initState = PState emptyRegisters 0

debugRegisters = M.insert 'a' 1 emptyRegisters 
debugState = PState debugRegisters 0

isMul (InstJump _) = False
isMul (InstOp op) = opType op == Mul

iterSteps :: V.Vector Inst -> StateT MyState IO Int
iterSteps insts = do
    MyState pstate mulCount i <- get
    case runStep insts pstate of
        Nothing -> return mulCount
        Just (pstateNext@(PState regs i), inst) -> do
            -- lift $ print inst
            -- lift $ print regs
            -- lift $ putStrLn ""
            MyState _ mulCount i <- get
            put $ MyState pstateNext mulCount (i + 1)
            when (isMul inst) $ do
                MyState pstate mulCount i <- get
                -- lift $ print regs
                put $ MyState pstate (mulCount + 1) i
                -- lift $ putStrLn $ "mul count: " ++ show (mulCount + 1)
            -- guard (mulCount < 5) 
            iterSteps insts

day23solution1 = do
    s <- pack <$> readFile "day23_input.txt"
    -- let q = "set a -23"
    let insts' = fromJust $ parseMaybe programP s
        insts = V.fromList insts'
    -- runStateT (iterSteps insts) (MyState debugState 0 0)  
    evalStateT (iterSteps insts) (MyState initState 0 0)  
