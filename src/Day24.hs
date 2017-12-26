{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Day24
where

import Text.Megaparsec
import Text.Megaparsec.Text
import Text.Megaparsec.Lexer as L
import Data.Text (Text, pack)
import Data.Maybe
import Data.Ord
import Data.List
import qualified Data.Set as Set


type Port = Int
type Component = (Port, Port)

-- | If the component can be matched to port return it together
-- with the other port
matchingPort :: Port -> Component -> Maybe (Component, Port)
matchingPort port component@(port1, port2)
    | port == port1 = Just (component, port2)
    | port == port2 = Just (component, port1)
    | otherwise = Nothing

componentsP :: Parser [Component]
componentsP =
    componentP `sepEndBy` eol
    where componentP = do
            port1 <- fromIntegral <$> L.integer
            char '/'
            port2 <- fromIntegral <$> L.integer
            return (port1, port2)

findBridges :: Set.Set Component -> Port -> [[Component]]
findBridges = findBridges' []
findBridges' path availableComps startPort =
    let l = matchingPort startPort <$> Set.elems availableComps
        l' = fromJust <$> filter isJust l
    in if null l'
       then [path]
       else concatMap f l'
           where f (comp, otherPort) = 
                    findBridges' (path ++ [comp])
                                 (Set.delete comp availableComps)
                                 otherPort

strength :: [Component] -> Int
strength path = sum $ fmap (uncurry (+)) path

day24solution1 = do
    s <- pack <$> readFile "day24_input.txt"
    let availableComps = Set.fromList $ fromJust $ parseMaybe componentsP s
    print $ maximum $ strength <$> findBridges availableComps 0

day24solution2 = do
    s <- pack <$> readFile "day24_input.txt"
    let availableComps = Set.fromList $ fromJust $ parseMaybe componentsP s
        maxPath = maximumBy (comparing length) (findBridges availableComps 0)
    print $ strength maxPath
