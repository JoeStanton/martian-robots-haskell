import Distribution.Simple
import Data.List

data Orientation = North | East | South | West deriving(Show)
charToOrientation "N" = North
charToOrientation "E" = East
charToOrientation "S" = South
charToOrientation "W" = West

data Instruction = TurnLeft | TurnRight | MoveForward deriving(Show)
charToInstruction 'L' = TurnLeft
charToInstruction 'R' = TurnRight
charToInstruction 'F' = MoveForward

data Robot = Robot {
  x :: Int,
  y :: Int,
  orientation :: Orientation
} deriving(Show)

main = do putStrLn "Martian Robots"
          inputFile <- readFile "example.txt"
          let input = lines inputFile
              bounds = parseBounds $ head input
              unparsed = chunks 2 $ tail input
              robotPairs = map parse unparsed
          print robotPairs
          print (map processInstructions robotPairs)

chunks n xs = takeWhile (not.null) $ unfoldr (Just . splitAt n) xs

parseBounds :: String -> (Int, Int)
parseBounds input = do
  let components = words input
      first = readInt $ head components
      second = readInt $ last components
  (first, second)

parseRobot :: String -> Robot
parseRobot input = do
  let components = words input
      first = readInt $ components !! 0
      second = readInt $ components !! 1
      orientation = charToOrientation $ components !! 2
  Robot first second orientation

parse :: [String] -> (Robot, String)
parse l = (parseRobot $ l !! 0, l !! 1)

processInstructions :: (Robot, String) -> Robot
processInstructions (robot, instructions) = foldr lookupAndExecute robot instructions

lookupAndExecute :: Char -> Robot -> Robot
lookupAndExecute instruction robot = processInstruction robot $ charToInstruction instruction

processInstruction Robot {x = x, y = y, orientation = o} TurnLeft = case o of
                                          North -> Robot x y West
                                          West  -> Robot x y South
                                          South -> Robot x y East
                                          East  -> Robot x y North

processInstruction Robot {x = x, y = y, orientation = o} TurnRight = case o of
                                          North -> Robot x y East
                                          East  -> Robot x y South
                                          South -> Robot x y West
                                          West  -> Robot x y North

processInstruction Robot {x = x, y = y, orientation = o} MoveForward = case o of
                                            North -> Robot x (y + 1) North
                                            West  -> Robot (x - 1) y West
                                            South -> Robot x (y - 1) South
                                            East  -> Robot (x + 1) y East

readInt :: String -> Int
readInt = read
