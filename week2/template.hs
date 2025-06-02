-- Week 2: Lists and List Comprehensions
-- Complete the function implementations below

import System.IO
import Data.List
import Data.Maybe

-- Type definitions
type Day = String
type Time = String
type Category = String
type ActivityName = String
type Duration = Int
type Activity = (Day, Time, Category, ActivityName, Duration)

-- Parse a single line into an Activity
parseActivity :: String -> Activity
parseActivity line = undefined -- TODO: Implement this function

-- Parse all activities from input content
parseActivities :: String -> [Activity]
parseActivities content = undefined -- TODO: Implement this function

-- Count activities per day
countByDay :: [Activity] -> [(Day, Int)]
countByDay activities = undefined -- TODO: Implement this function

-- Calculate total time spent per category
timeByCategory :: [Activity] -> [(Category, Int)]
timeByCategory activities = undefined -- TODO: Implement this function

-- Find the busiest hour of the weekend
-- Return the day, start time, and number of activities happening
bustiestHour :: [Activity] -> (Day, Time, Int)
bustiestHour activities = undefined -- TODO: Implement this function

-- Format the results into a readable string
formatResults :: [(Day, Int)] -> [(Category, Int)] -> (Day, Time, Int) -> String
formatResults dayCount categoryTime busiest = undefined -- TODO: Implement this function

-- Main function
main :: IO ()
main = do
  content <- readFile "input.txt"
  let activities = parseActivities content
  let byDay = countByDay activities
  let byCategory = timeByCategory activities
  let busiest = bustiestHour activities
  putStrLn (formatResults byDay byCategory busiest)