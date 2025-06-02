-- Week 1: Getting Started with Haskell
-- Complete the function implementations below

import System.IO
import Data.List

-- Type alias for activities
type Activity = (String, Int)

-- Parse a single line of input into an Activity
parseActivity :: String -> Activity
parseActivity line = undefined -- TODO: Implement this function

-- Parse all activities from input
parseActivities :: String -> [Activity]
parseActivities content = undefined -- TODO: Implement this function

-- Find the activity that takes the longest time
findLongestActivity :: [Activity] -> Activity
findLongestActivity activities = undefined -- TODO: Implement this function

-- Find the activity that takes the shortest time
findShortestActivity :: [Activity] -> Activity
findShortestActivity activities = undefined -- TODO: Implement this function

-- Calculate the total time spent on all activities
calculateTotalTime :: [Activity] -> Int
calculateTotalTime activities = undefined -- TODO: Implement this function

-- Format the results into a readable string
formatResults :: Activity -> Activity -> Int -> String
formatResults longest shortest total = undefined -- TODO: Implement this function

-- Main function to run the program
main :: IO ()
main = do
  content <- readFile "input.txt"
  let activities = parseActivities content
  let longest = findLongestActivity activities
  let shortest = findShortestActivity activities
  let total = calculateTotalTime activities
  putStrLn (formatResults longest shortest total)