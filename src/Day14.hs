{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Day14
where

import Data.Char
import Day10 (knotHashNum)

binary :: Int -> [Int]
binary n =
    if n == 0 then [] else
    let i = n `rem` 2
    in i : binary (n `div` 2)

ones :: Int -> Int
ones = sum . binary

hashOnes s =
    let xs = knotHashNum (fmap ord s)
    in sum $ fmap ones xs

enumStrings :: String -> [String]
enumStrings s = fmap (\ i -> s ++ "-" ++ show i) [0..127]

day14solution1 =
    sum $ fmap hashOnes (enumStrings "stpzcrnm")
    
