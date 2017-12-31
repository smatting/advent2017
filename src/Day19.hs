module Day19
where

import qualified Data.Map.Lazy as M
import Data.Maybe
import Data.Char

data Dir = DLeft | DRight | DUp | DDown deriving (Show, Eq)
type Pos = (Int, Int)
type Map = M.Map (Int, Int) Char

loadmap :: IO Map
loadmap = do
  contents <- readFile "day19_input.txt"
  return $ M.fromList $ do 
    (y, line) <- zip [0..] $ lines contents
    (x, c) <- zip [0..] line
    return ((x, y), c)

findEntry m =
  head $ filter ((== '|') . (m M.!)) [(x, 0) | x <- [0..]]

move (x, y) DLeft  = (x - 1, y)
move (x, y) DRight = (x + 1, y)
move (x, y) DUp    = (x, y - 1)
move (x, y) DDown  = (x, y + 1)

go :: Map -> Pos -> Dir -> Maybe (Pos, Char, Dir)
go m pos dir  = do
  let pos' = move pos dir
  c <- M.lookup pos' m 
  if c /= ' ' then Just (pos', c, dir) else Nothing

step :: Map -> Pos -> Dir -> Maybe (Pos, Char, Dir)
step m pos dir = do
  let dirs = filter isJust $ go m pos <$> getDirs dir
  if null dirs then Nothing else head dirs
  where
    getDirs DUp = [DUp, DLeft, DRight]
    getDirs DLeft = [DLeft, DDown, DUp]
    getDirs DDown = [DDown, DLeft, DRight]
    getDirs DRight = [DRight, DDown, DUp]

walk :: Map -> String -> Pos -> Dir -> String
walk m s pos dir =
  case step m pos dir of
    Just (pos', char, dir') -> walk m (char:s) pos' dir'
    Nothing -> reverse s

day19solution = do
  m <- loadmap
  let pos = findEntry m
      s = walk m "" pos DDown
  putStrLn $ filter isAlpha s
  print $ length s + 1
