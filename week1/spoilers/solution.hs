-- Week 1: Solution
-- A complete implementation of the Week 1 exercise

import System.IO
import Data.List

-- Type alias for activities
type Activity = (String, Int)

-- Parse a single line of input into an Activity
parseActivity :: String -> Activity
parseActivity line = 
  let words' = words line
      name = init words'
      time = read (last words') :: Int
  in (unwords name, time)

-- Parse all activities from input
parseActivities :: String -> [Activity]
parseActivities content = map parseActivity (lines content)

-- Find the activity that takes the longest time
findLongestActivity :: [Activity] -> Activity
findLongestActivity = maximumBy (\(_, t1) (_, t2) -> compare t1 t2)

-- Find the activity that takes the shortest time
findShortestActivity :: [Activity] -> Activity
findShortestActivity = minimumBy (\(_, t1) (_, t2) -> compare t1 t2)

-- Calculate the total time spent on all activities
calculateTotalTime :: [Activity] -> Int
calculateTotalTime = sum . map snd

-- Format the results into a readable string
formatResults :: Activity -> Activity -> Int -> String
formatResults (longestName, longestTime) (shortestName, shortestTime) total =
  "Longest activity: " ++ longestName ++ " (" ++ show longestTime ++ " minutes)\n" ++
  "Shortest activity: " ++ shortestName ++ " (" ++ show shortestTime ++ " minutes)\n" ++
  "Total time: " ++ show total ++ " minutes"

-- Main function to run the program
main :: IO ()
main = do
  content <- readFile "input.txt"
  let activities = parseActivities content
  let longest = findLongestActivity activities
  let shortest = findShortestActivity activities
  let total = calculateTotalTime activities
  putStrLn (formatResults longest shortest total)