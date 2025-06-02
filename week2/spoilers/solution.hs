-- Week 2: Solution
-- A complete implementation of the Week 2 exercise

import System.IO
import Data.List
import Data.Maybe
import Data.Ord (comparing)

-- Type definitions
type Day = String
type Time = String
type Category = String
type ActivityName = String
type Duration = Int
type Activity = (Day, Time, Category, ActivityName, Duration)

-- Parse a single line into an Activity
parseActivity :: String -> Activity
parseActivity line =
  let parts = words line
      day = head parts
      time = parts !! 1
      category = parts !! 2
      name = parts !! 3
      duration = read (last parts) :: Int
  in (day, time, category, name, duration)

-- Parse all activities from input content
parseActivities :: String -> [Activity]
parseActivities content = map parseActivity (lines content)

-- Count activities per day
countByDay :: [Activity] -> [(Day, Int)]
countByDay activities =
  let days = nub $ map (\(d, _, _, _, _) -> d) activities
      countForDay day = (day, length [a | a@(d, _, _, _, _) <- activities, d == day])
  in map countForDay days

-- Calculate total time spent per category
timeByCategory :: [Activity] -> [(Category, Int)]
timeByCategory activities =
  let categories = nub $ map (\(_, _, c, _, _) -> c) activities
      timeForCategory cat = (cat, sum [dur | (_, _, c, _, dur) <- activities, c == cat])
  in map timeForCategory categories

-- Helper to check if an activity is ongoing at a specific hour
isOngoingAt :: Day -> String -> Activity -> Bool
isOngoingAt checkDay hourStr (day, timeStr, _, _, duration)
  | day /= checkDay = False
  | otherwise =
      let startHour = read (takeWhile (/= ':') timeStr) :: Int
          endHour = startHour + (duration `div` 60) + (if duration `mod` 60 > 0 then 1 else 0)
          checkHour = read (takeWhile (/= ':') hourStr) :: Int
      in checkHour >= startHour && checkHour < endHour

-- Find the busiest hour of the weekend
bustiestHour :: [Activity] -> (Day, Time, Int)
bustiestHour activities =
  let days = nub $ map (\(d, _, _, _, _) -> d) activities
      hours = ["0" ++ show h ++ ":00" | h <- [8..9]] ++ [show h ++ ":00" | h <- [10..20]]
      timeSlots = [(d, h) | d <- days, h <- hours]
      countActivities (d, h) = (d, h, length [a | a <- activities, isOngoingAt d h a])
      allCounts = map countActivities timeSlots
  in maximumBy (comparing (\(_, _, count) -> count)) allCounts

-- Format the results into a readable string
formatResults :: [(Day, Int)] -> [(Category, Int)] -> (Day, Time, Int) -> String
formatResults dayCount categoryTime (busiestDay, busiestTime, busiestCount) =
  let dayLines = unlines [day ++ ": " ++ show count ++ " activities" | (day, count) <- dayCount]
      categoryLines = unlines ["  " ++ category ++ ": " ++ show (time `div` 60) ++ 
                              " hour" ++ (if time `div` 60 /= 1 then "s" else "") | 
                              (category, time) <- categoryTime]
      busiestLine = busiestDay ++ " " ++ busiestTime ++ " (" ++ show busiestCount ++ " activities)"
  in "Activities by day:\n" ++ dayLines ++ 
     "\nTime by category:\n" ++ categoryLines ++ 
     "\nBusiest time: " ++ busiestLine

-- Main function
main :: IO ()
main = do
  content <- readFile "input.txt"
  let activities = parseActivities content
  let byDay = countByDay activities
  let byCategory = timeByCategory activities
  let busiest = bustiestHour activities
  putStrLn (formatResults byDay byCategory busiest)