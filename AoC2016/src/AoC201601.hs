module AoC201601
    ( runDay,
    ) where

import Data.List
import Text.Parsec
import Text.Parsec.String

runDay :: IO ()
runDay = do
  putStrLn $ "1) Easter Bunny HQ is " ++ (show calculateDistance) ++ " blocks away"
  putStrLn $ "1) The first crossover in the paths happens at " ++ show findFirstOverlap ++ ", which is " ++ show findFirstOverlapDistance ++ " blocks away"

data Turn = LeftTurn | RightTurn deriving Show
data Instruction = Instruction { turn :: Turn, distance :: Int } deriving Show
data InstructionSet = InstructionSet { instructions :: [Instruction] } deriving Show
data Location = Location { xLocation :: Int, yLocation :: Int } deriving (Show, Eq)
data Position = Position { direction :: Direction,  location :: Location} deriving Show
data Direction = North | South | East | West deriving Show

-- Part 1
movePositions :: Position -> Direction -> Int -> [Position]
movePositions _ _ 0 = []
movePositions p dir d = let
    newX = moveX dir (xLocation $ location p) 1
    newY = moveY dir (yLocation $ location p) 1
    newLocation = Location newX newY
    nextPosition = Position dir newLocation
  in nextPosition : (movePositions nextPosition dir $ d - 1)

calculateDistance :: String
calculateDistance =
  case parsedMoves of
    Left msg -> show msg
    Right is -> show $ blockDistance $ makeMoves is

blockDistance :: Position -> Int
blockDistance p = locationDistance $ location p

locationDistance :: Location -> Int
locationDistance l = (abs $ xLocation l) + (abs $ yLocation l)

makeMoves :: InstructionSet -> Position
makeMoves is = foldl (\acc i -> move acc (turn i) (distance i)) startingPosition (instructions is)

startingPosition :: Position
startingPosition = Position North $ Location 0 0

move :: Position -> Turn -> Int -> Position
move p t d = let
    newDirection = nextDirection (direction p) t
    newX = moveX newDirection (xLocation $ location p) d
    newY = moveY newDirection (yLocation $ location p) d
  in Position newDirection $ Location newX newY

moveX :: Direction -> Int -> Int -> Int
moveX East x d = x + d
moveX West x d = x - d
moveX _ x _ = x

moveY :: Direction -> Int -> Int -> Int
moveY North y d = y + d
moveY South y d = y - d
moveY _ y _ = y

nextDirection :: Direction -> Turn -> Direction
nextDirection North LeftTurn = West
nextDirection North RightTurn = East
nextDirection West LeftTurn = South
nextDirection West RightTurn = North
nextDirection South LeftTurn = East
nextDirection South RightTurn = West
nextDirection East LeftTurn = North
nextDirection East RightTurn = South

-- Part 2
findFirstOverlapDistance :: String
findFirstOverlapDistance = 
  case parsedMoves of
    Left msg -> show msg
    Right is -> let
      in show $ locationDistance $ head $ duplicates $ getJourney is

findFirstOverlap :: String
findFirstOverlap =
  case parsedMoves of
    Left msg -> show msg
    Right is -> let
      in show $ head $ duplicates $ getJourney is

duplicates :: Eq a => [a] -> [a]
duplicates xs = nub $ filter (isDuplicate xs) xs

isDuplicate :: Eq a => [a] -> a -> Bool
isDuplicate xs x
  | count >= 2 = True
  | otherwise = False
  where
    count = length $ filter (== x) xs

visitedLocations is = nub $ getJourney is

getJourney is = reverse $ extractLocations $ head $ reverse $ calculateBlockJourney $ instructions is

extractLocations :: [Position] -> [Location]
extractLocations xs = map (\x -> location x) xs

calculateBlockJourney :: [Instruction] -> [[Position]]
calculateBlockJourney xs = scanl(\acc x -> reverse (movePositions (head acc) (nextDirection (direction $ head acc) (turn x)) (distance x)) ++ acc) [startingPosition] xs

-- Parser

parsedMoves :: Either ParseError InstructionSet
parsedMoves = parse instructionSetParser "test" inputDirections
parsedMoves' = parse instructionSetParser "test" smallDirections

instructionSetParser :: Parser InstructionSet
instructionSetParser = do
  xs <- many1 instructionParser
  return $ InstructionSet xs

instructionParser :: Parser Instruction
instructionParser = let
    cA a = case a of
      'L' -> LeftTurn
      'R' -> RightTurn
  in do 
    t <- letter
    d <- many1 digit
    optional $ char ','
    optional spaces
    return $ Instruction (cA t) (read d :: Int)

-- Input data
smallDirections :: String
smallDirections = "R8, R4, R4, R8"

inputDirections :: String
inputDirections = "L5, R1, L5, L1, R5, R1, R1, L4, L1, L3, R2, R4, L4, L1, L1, R2, R4, R3, L1, R4, L4, L5, L4, R4, L5, R1, R5, L2, R1, R3, L2, L4, L4, R1, L192, R5, R1, R4, L5, L4, R5, L1, L1, R48, R5, R5, L2, R4, R4, R1, R3, L1, L4, L5, R1, L4, L2, L5, R5, L2, R74, R4, L1, R188, R5, L4, L2, R5, R2, L4, R4, R3, R3, R2, R1, L3, L2, L5, L5, L2, L1, R1, R5, R4, L3, R5, L1, L3, R4, L1, L3, L2, R1, R3, R2, R5, L3, L1, L1, R5, L4, L5, R5, R2, L5, R2, L1, L5, L3, L5, L5, L1, R1, L4, L3, L1, R2, R5, L1, L3, R4, R5, L4, L1, R5, L1, R5, R5, R5, R2, R1, R2, L5, L5, L5, R4, L5, L4, L4, R5, L2, R1, R5, L1, L5, R4, L3, R4, L2, R3, R3, R3, L2, L2, L2, L1, L4, R3, L4, L2, R2, R5, L1, R2"
