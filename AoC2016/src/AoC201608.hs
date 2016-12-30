{-# LANGUAGE QuasiQuotes #-}

module AoC201608
  ( runDay,
  ) where

import Data.List (sortBy)
import Data.List.Split (chunksOf)
import Data.Maybe (isJust, fromJust)
import Data.Char (digitToInt)
import Str
import Text.Parsec
import Text.Parsec.String

runDay :: IO ()
runDay = do
  let part1Result = execute fullInput fullGrid
  let part2Result = execute' fullInput fullGrid
  putStrLn $ "8) There are " ++ part1Result ++ " filled pixels."
  putStrLn "8) Letters are:"
  putStrLn ""
  putStr part2Result
  putStrLn ""
  putStrLn ""

-- Part 2

execute' :: String -> [((Int, Int), Int)] -> String
execute' xs grid =
    case parsedInstructions xs of
    Prelude.Left msg -> show msg
    Prelude.Right instructions -> unlines $ chunksOf 50 $ convertGridValuesForPrinting $ sortGrid $ foldl (\acc x -> runInstruction acc x) grid instructions

convertGridValuesForPrinting :: (Eq a, Num a) => [(t, a)] -> String
convertGridValuesForPrinting grid = map (\(_, v) -> if v == 0 then ' ' else '#') grid

sortGrid :: (Ord a) => [((a, a), t)] -> [((a, a), t)]
sortGrid grid = sortBy (\((x, y), v) ((x', y'), v') -> if y `compare` y' == EQ then x `compare` x' else y `compare` y') grid

-- Part 1

execute :: String -> [((Int, Int), Int)] -> String
execute xs grid =
  case parsedInstructions xs of
    Prelude.Left msg -> show msg
    Prelude.Right instructions -> show $ countFilled $ foldl (\acc x -> runInstruction acc x) grid instructions

data Instruction =
  Rect { w :: Int, h :: Int } |
  Rotate { direction :: Direction, index :: Int, distance :: Int }
  deriving Show
data Direction = Row | Column deriving Show

countFilled grid = foldl (\acc (_, v) -> acc + v) 0 grid

runInstruction :: [((Int, Int), Int)] -> Instruction -> [((Int, Int), Int)]
runInstruction grid i = update grid $ createChangeset grid i

createChangeset :: [((Int, Int), Int)] -> Instruction -> [((Int, Int), Int)]
createChangeset grid i = case i of
    Rect width height -> fill width height
    Rotate Row i d -> shiftRow d $ extractRow i grid
    Rotate Column i d -> shiftColumn d $ extractColumn i grid

-- Drawing

shiftColumn :: Int -> [((t1, Int), t)] -> [((t1, Int), t)]
shiftColumn distance col = map (\((x, y), v) -> ((x, ((y + distance) `mod` (length col))), v)) col
shiftRow :: Int -> [((Int, t), t)] -> [((Int, t), t)]
shiftRow distance row = map (\((x, y), v) -> ((((x + distance) `mod` (length row)), y), v)) row

extractColumn :: Eq a => a -> [((a, t1), t)] -> [((a, t1), t)]
extractColumn index grid = filter (\((x, _), _) -> x == index) grid

extractRow :: Eq a => a -> [((t1, a), t)] -> [((t1, a), t)]
extractRow index grid = filter (\((_, y), _) -> y == index) grid

fill :: (Enum t, Num t) => t -> t -> [((t, t), Int)]
fill x y = let
  newRect = genGrid x y ones
  in newRect

update :: Eq a => [(a, t)] -> [(a, t)] -> [(a, t)]
update grid changeset = map (\(k, v) -> update' changeset k v) grid

update' :: Eq a => [(a, t)] -> a -> t -> (a, t)
update' changeset k v = if (isJust $ lookup k changeset) then (k, (fromJust $ lookup k changeset)) else (k, v)

-- Screen generation

testGrid :: [((Int, Int), Int)]
testGrid = genGrid 7 3 zeroes

fullGrid :: [((Int, Int), Int)]
fullGrid = genGrid 50 6 zeroes

genGrid :: (Enum t, Num t) => t -> t -> [b] -> [((t, t), b)]
genGrid x y fill = zip[(i, j) | i <- [0..(x - 1)], j <- [0..(y - 1)]] fill

ones :: [Int]
ones = repeat 1

zeroes :: [Int]
zeroes = repeat 0

-- Parsers

parsedInstructions :: String -> Either ParseError [Instruction]
parsedInstructions xs = parse instructionsParser "test" xs

instructionsParser :: Parser [Instruction]
instructionsParser = do
  xs <- many1 instructionParser
  return xs

instructionParser :: Parser Instruction
instructionParser = do
  i <- try rectParser <|> rotationParser
  return i

rectParser :: Parser Instruction
rectParser = do
  string "rect"
  many1 space
  w <- many1 digit
  char 'x'
  h <- many1 digit
  optional endOfLine
  return $ Rect (read w :: Int) (read h :: Int)

rotationParser :: Parser Instruction
rotationParser = let
    cD d = case d of
      "column" -> Column
      "row" -> Row
  in do
    string "rotate"
    many1 space
    direction <- many1 letter
    many1 space
    oneOf "xy"
    char '='
    i <- many1 digit
    many1 space
    string "by"
    many1 space
    d <- many1 digit
    optional endOfLine
    return $ Rotate (cD direction) (read i :: Int) (read d :: Int)

-- Input data

smallInput :: String
smallInput = [str|rect 3x2
rotate column x=1 by 1
rect 1x2
rotate row y=0 by 4
rotate column x=1 by 1|]

fullInput :: String
fullInput = [str|rect 1x1
rotate row y=0 by 10
rect 1x1
rotate row y=0 by 10
rect 1x1
rotate row y=0 by 5
rect 1x1
rotate row y=0 by 3
rect 2x1
rotate row y=0 by 4
rect 1x1
rotate row y=0 by 3
rect 1x1
rotate row y=0 by 2
rect 1x1
rotate row y=0 by 3
rect 2x1
rotate row y=0 by 2
rect 1x1
rotate row y=0 by 3
rect 2x1
rotate row y=0 by 5
rotate column x=0 by 1
rect 4x1
rotate row y=1 by 12
rotate row y=0 by 10
rotate column x=0 by 1
rect 9x1
rotate column x=7 by 1
rotate row y=1 by 3
rotate row y=0 by 2
rect 1x2
rotate row y=1 by 3
rotate row y=0 by 1
rect 1x3
rotate column x=35 by 1
rotate column x=5 by 2
rotate row y=2 by 5
rotate row y=1 by 5
rotate row y=0 by 2
rect 1x3
rotate row y=2 by 8
rotate row y=1 by 10
rotate row y=0 by 5
rotate column x=5 by 1
rotate column x=0 by 1
rect 6x1
rotate row y=2 by 7
rotate row y=0 by 5
rotate column x=0 by 1
rect 4x1
rotate column x=40 by 2
rotate row y=2 by 10
rotate row y=0 by 12
rotate column x=5 by 1
rotate column x=0 by 1
rect 9x1
rotate column x=43 by 1
rotate column x=40 by 2
rotate column x=38 by 1
rotate column x=15 by 1
rotate row y=3 by 35
rotate row y=2 by 35
rotate row y=1 by 32
rotate row y=0 by 40
rotate column x=32 by 1
rotate column x=29 by 1
rotate column x=27 by 1
rotate column x=25 by 1
rotate column x=23 by 2
rotate column x=22 by 1
rotate column x=21 by 3
rotate column x=20 by 1
rotate column x=18 by 3
rotate column x=17 by 1
rotate column x=15 by 1
rotate column x=14 by 1
rotate column x=12 by 1
rotate column x=11 by 3
rotate column x=10 by 1
rotate column x=9 by 1
rotate column x=8 by 2
rotate column x=7 by 1
rotate column x=4 by 1
rotate column x=3 by 1
rotate column x=2 by 1
rotate column x=0 by 1
rect 34x1
rotate column x=44 by 1
rotate column x=24 by 1
rotate column x=19 by 1
rotate row y=1 by 8
rotate row y=0 by 10
rotate column x=8 by 1
rotate column x=7 by 1
rotate column x=6 by 1
rotate column x=5 by 2
rotate column x=3 by 1
rotate column x=2 by 1
rotate column x=1 by 1
rotate column x=0 by 1
rect 9x1
rotate row y=0 by 40
rotate column x=43 by 1
rotate row y=4 by 10
rotate row y=3 by 10
rotate row y=2 by 5
rotate row y=1 by 10
rotate row y=0 by 15
rotate column x=7 by 2
rotate column x=6 by 3
rotate column x=5 by 2
rotate column x=3 by 2
rotate column x=2 by 4
rotate column x=0 by 2
rect 9x2
rotate row y=3 by 47
rotate row y=0 by 10
rotate column x=42 by 3
rotate column x=39 by 4
rotate column x=34 by 3
rotate column x=32 by 3
rotate column x=29 by 3
rotate column x=22 by 3
rotate column x=19 by 3
rotate column x=14 by 4
rotate column x=4 by 3
rotate row y=4 by 3
rotate row y=3 by 8
rotate row y=1 by 5
rotate column x=2 by 3
rotate column x=1 by 3
rotate column x=0 by 2
rect 3x2
rotate row y=4 by 8
rotate column x=45 by 1
rotate column x=40 by 5
rotate column x=26 by 3
rotate column x=25 by 5
rotate column x=15 by 5
rotate column x=10 by 5
rotate column x=7 by 5
rotate row y=5 by 35
rotate row y=4 by 42
rotate row y=2 by 5
rotate row y=1 by 20
rotate row y=0 by 45
rotate column x=48 by 5
rotate column x=47 by 5
rotate column x=46 by 5
rotate column x=43 by 5
rotate column x=41 by 5
rotate column x=38 by 5
rotate column x=37 by 5
rotate column x=36 by 5
rotate column x=33 by 1
rotate column x=32 by 5
rotate column x=31 by 5
rotate column x=30 by 1
rotate column x=28 by 5
rotate column x=27 by 5
rotate column x=26 by 5
rotate column x=23 by 1
rotate column x=22 by 5
rotate column x=21 by 5
rotate column x=20 by 1
rotate column x=17 by 5
rotate column x=16 by 5
rotate column x=13 by 1
rotate column x=12 by 3
rotate column x=7 by 5
rotate column x=6 by 5
rotate column x=3 by 1
rotate column x=2 by 3|]
