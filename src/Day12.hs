module Day12
where

import Text.Megaparsec
import Text.Megaparsec.Text
import Text.Megaparsec.Lexer as L
import Data.Maybe
import Data.Text (Text, pack)
import Data.Tuple (swap)
import Data.HashSet as Set
import Data.HashMap.Lazy as Map

pipesP :: Parser (Int, [Int])
pipesP = do
    n1 <- fromIntegral <$> L.integer
    string " <-> "
    n2s <- (fromIntegral <$> L.integer) `sepBy` string ", "
    return (n1, n2s)
    
problemP = Map.fromList <$> pipesP `sepEndBy` eol

allNodes :: HashSet Int
allNodes = Set.fromList [0..1999]

findAll :: HashMap Int [Int] -> HashSet Int -> Int -> HashSet Int
findAll connections = findAll' 
    where findAll' availableNodes query =
            let nodes = (Set.fromList (connections ! query)
                        `Set.intersection`
                        availableNodes)
                        `Set.difference` Set.singleton query
                availableNodes' = availableNodes `Set.difference` nodes
            in
                nodes
                `Set.union`
                Set.unions (findAll' availableNodes' <$> Set.toList nodes)

countGroups connections availableNodes n =
    if Set.null availableNodes then n else
        let x = head $ Set.toList availableNodes
            nodes = findAll connections availableNodes x
            availableNodes' = availableNodes `Set.difference` nodes `Set.difference` Set.singleton x
        in countGroups connections availableNodes' (n + 1)
    
day12solution1 = do
    s <- pack <$> readFile "day12_input.txt"
    let connections = fromJust $ parseMaybe problemP s
        result = findAll connections allNodes 0
    print $ length result

day12solution2 = do
    s <- pack <$> readFile "day12_input.txt"
    let connections = fromJust $ parseMaybe problemP s
        result = countGroups connections allNodes 0
    print result
