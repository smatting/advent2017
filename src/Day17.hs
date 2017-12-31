module Day17
where

import qualified Data.Sequence as S
import Data.Foldable as F


type Position = Int
type Seq = S.Seq Int
data State = S Position Seq deriving Show


startState :: State
startState = S 0 (S.fromList [0])

makeStep stepSize = step
  where
    step :: State -> Int -> State
    step (S pos seq) k =
      let pos'  = (pos + stepSize) `mod` S.length seq
          seq'  = insertAfter pos' k seq
          pos'' = (pos' + 1) `mod` S.length seq'
      in S pos'' seq'

insertAfter :: Int -> Int -> Seq -> Seq
insertAfter i x seq =
  let (before, after) = S.splitAt (i + 1) seq
  in before S.>< (x S.<| after)


foo = insertAfter 0 33 (S.fromList [0])

getNext :: State -> Int
getNext (S pos seq) =
  let i = (pos + 1) `mod` S.length seq
  in seq `S.index` i

day17solution1 =
  let endState = F.foldl' (makeStep 345) startState [1..2017]
  in getNext endState

data Prob2 = P Position Position Int Int

instance Show Prob2
  where show (P posZero pos len nAfterZero) =
          unwords ["pos: " ++ show pos,
                   "posZero: " ++ show posZero,
                   "nAfterZero: " ++ show nAfterZero]

prob2start = P 0 0 1 0

makeStep2 stepSize = step2
  where
    step2 :: Prob2 -> Int -> Prob2
    step2 (P posZero pos len nAfterZero) k =
      let pos'        = (pos + stepSize) `mod` len
          nAfterZero' = if pos' == posZero then k else nAfterZero
          posZero'    = if pos' < posZero then posZero + 1 else posZero
          len'        = len + 1
          pos''       = (pos' + 1) `mod` len'
      in seq posZero' $ seq pos'' $ seq len' $ seq nAfterZero' $ P posZero' pos'' len' nAfterZero'


day17solution2 = F.foldl' (makeStep2 345) prob2start [1..50000000]
