module Day7
where


import Prelude hiding (foldr)
import Text.Megaparsec
import qualified Data.Map.Lazy as M
import Text.Megaparsec.String
import Text.Megaparsec.Lexer as L
import Data.Maybe
import Data.Text (Text, pack)
import Data.Foldable

data Program = Program {
    name :: Text,
    weight :: Int,
    children :: Maybe [Text]
} deriving (Show)
    
programsP :: Parser [Program]
programsP =
    do
        name <- pack <$> some alphaNumChar
        skipMany $ char ' '
        weight <- between (char '(') (char ')') $ fromIntegral <$> integer
        optional $ string " -> "
        children <- optional $ (pack <$> some alphaNumChar) `sepBy1` string ", "
        return Program {name=name, weight=weight, children=children}
    `sepEndBy` eol

makeParent :: [Program] -> M.Map Text Text
makeParent = foldr f M.empty
    where f program m =
            let
                parentName = name program
                g Nothing m = m
                g (Just names) m = foldr h m names
                h name = M.insert name parentName
            in g (children program) m


origin :: M.Map Text Text -> Text -> Text
origin parent child =
    case M.lookup child parent 
        of Just p -> origin parent p
           Nothing -> child


indexPrograms :: [Program] -> M.Map Text Program
indexPrograms = foldr f M.empty
    where f p = M.insert (name p) p

allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame (x:xs) = all (x ==) xs

towerWeight ::  M.Map Text Program -> Program -> Int
towerWeight programs program =
    case children program
        of Nothing -> weight program
           Just names -> weight program + sum (towerWeight programs <$> childs)
               where childs = (programs M.!) <$> names

day7solution1 = do
    s <- readFile "day7_input.txt"
    let programs = fromJust $ parseMaybe programsP s
        parent = makeParent programs
        name = head $ M.keys parent
    print $ origin parent name


balanced :: M.Map Text Program -> Program -> Bool
balanced programs program =
    case fmap (programs M.!) <$> children program
        of Nothing -> True
           Just ps -> allSame (towerWeight programs <$> ps)

printWeights programs program = do
    let names = fromMaybe [] (children program)
        childs = (programs M.!) <$> names
    print (name program)
    sequence $ (\p -> print (weight p, towerWeight programs p)) <$> childs

    
day7solution2 = do
    s <- readFile "day7_input.txt"
    let programs = indexPrograms $ fromJust $ parseMaybe programsP s
        program = head $ M.elems programs
        unbalancedPrograms = filter (not . balanced programs) (M.elems programs)
    sequence_ $ printWeights programs <$> unbalancedPrograms
