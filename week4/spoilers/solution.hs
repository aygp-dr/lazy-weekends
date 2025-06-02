-- Week 4: Solution
-- A complete implementation of the Week 4 exercise

import System.IO
import Data.List
import Data.Maybe
import Text.Printf
import Data.Function (on)

-- Type definitions
type Date = String
type Category = String
type Amount = Double
type TimeOfDay = String
type Expense = (Date, Category, Amount, TimeOfDay)

-- Parse a single line into an Expense
parseExpense :: String -> Expense
parseExpense line =
  let parts = splitOn ',' line
      date = parts !! 0
      category = parts !! 1
      amount = read (parts !! 2) :: Double
      timeOfDay = parts !! 3
  in (date, category, amount, timeOfDay)

-- Helper function to split a string by delimiter
splitOn :: Char -> String -> [String]
splitOn delimiter = foldr f [[]]
  where
    f c acc@(x:xs)
      | c == delimiter = []:acc
      | otherwise = (c:x):xs

-- Parse all expenses from input content
parseExpenses :: String -> [Expense]
parseExpenses content = map parseExpense (lines content)

-- Extract the weekend date (Saturday) from a date
getWeekendDate :: Date -> String
getWeekendDate date =
  let year = take 4 date
      month = take 2 $ drop 5 date
      day = take 2 $ drop 8 date
  in year ++ "-" ++ month ++ "-" ++ day

-- Get weekend identifier (e.g., "2023-06-03/04")
getWeekendIdentifier :: Date -> String
getWeekendIdentifier date =
  let dateComponents = splitOn '-' date
      year = dateComponents !! 0
      month = dateComponents !! 1
      day = read (dateComponents !! 2) :: Int
      saturdayDay = if mod day 7 == 0 then day - 1 else day - (mod day 7) + 6
      sundayDay = saturdayDay + 1
      saturdayStr = if saturdayDay < 10 then "0" ++ show saturdayDay else show saturdayDay
      sundayStr = if sundayDay < 10 then "0" ++ show sundayDay else show sundayDay
  in year ++ "-" ++ month ++ "-" ++ saturdayStr ++ "/" ++ sundayStr

-- Group expenses by weekend (use recursion)
groupByWeekend :: [Expense] -> [(String, [Expense])]
groupByWeekend [] = []
groupByWeekend expenses =
  let sortedExpenses = sortOn (\(date, _, _, _) -> date) expenses
      weekendIds = nub $ map (\(date, _, _, _) -> getWeekendIdentifier date) sortedExpenses
      groupForWeekend wid = (wid, filter (\(date, _, _, _) -> getWeekendIdentifier date == wid) expenses)
  in map groupForWeekend weekendIds

-- Calculate total spending for a list of expenses (use fold)
totalSpending :: [Expense] -> Amount
totalSpending = foldr (\(_, _, amount, _) acc -> amount + acc) 0.0

-- Group expenses by category and sum amounts (use higher-order functions)
spendingByCategory :: [Expense] -> [(Category, Amount)]
spendingByCategory expenses =
  let categories = nub $ map (\(_, cat, _, _) -> cat) expenses
      sumForCategory cat = sum [amount | (_, c, amount, _) <- expenses, c == cat]
  in [(cat, sumForCategory cat) | cat <- categories]

-- Group expenses by time of day and sum amounts (use higher-order functions)
spendingByTimeOfDay :: [Expense] -> [(TimeOfDay, Amount)]
spendingByTimeOfDay expenses =
  let times = nub $ map (\(_, _, _, time) -> time) expenses
      sumForTime t = sum [amount | (_, _, amount, time) <- expenses, time == t]
  in [(time, sumForTime time) | time <- times]

-- Find the highest expense (use higher-order functions)
highestExpense :: [Expense] -> Expense
highestExpense = maximumBy (compare `on` (\(_, _, amount, _) -> amount))

-- Categorize expenses for better reporting
categorizeExpense :: Category -> String
categorizeExpense cat
  | cat `elem` ["Coffee", "Lunch", "Dinner", "Brunch", "Drinks", "Takeout", "Bakery", "Ice Cream", "Cafe", "Delivery", "Groceries"] = "Food & Drink"
  | cat `elem` ["Movie", "Museum", "Theater", "Park", "Gym"] = "Entertainment"
  | cat `elem` ["Bookstore", "Shopping"] = "Shopping"
  | cat `elem` ["Hardware Store", "Pharmacy", "Gas"] = "Essentials"
  | cat `elem` ["Uber"] = "Transportation"
  | cat `elem` ["Farmers Market"] = "Groceries"
  | otherwise = "Other"

-- Group expenses by high-level category
spendingByHighLevelCategory :: [Expense] -> [(String, Amount)]
spendingByHighLevelCategory expenses =
  let highLevelCategories = nub $ map (\(_, cat, _, _) -> categorizeExpense cat) expenses
      sumForCategory cat = sum [amount | (_, c, amount, _) <- expenses, categorizeExpense c == cat]
  in [(cat, sumForCategory cat) | cat <- highLevelCategories]

-- Format the results for a single weekend
formatWeekendAnalysis :: (String, [Expense]) -> String
formatWeekendAnalysis (weekend, expenses) =
  let total = totalSpending expenses
      byCategory = spendingByHighLevelCategory expenses
      byTime = spendingByTimeOfDay expenses
      (_, highestCat, highestAmount, _) = highestExpense expenses
      (_, _, _, highestTimeRaw) = maximumBy (compare `on` (\(_, _, amount, _) -> amount)) expenses
      
      formatCategory (cat, amount) = "  " ++ cat ++ ": $" ++ printf "%.2f" amount
      categoryLines = unlines $ map formatCategory byCategory
      
      formatTime (time, amount) = "  " ++ time ++ ": $" ++ printf "%.2f" amount
      timeLines = unlines $ map formatTime byTime
      
  in "Weekend of " ++ weekend ++ "\n" ++
     "Total spending: $" ++ printf "%.2f" total ++ "\n" ++
     "By category:\n" ++ categoryLines ++
     "Highest expense: " ++ highestCat ++ " ($" ++ printf "%.2f" highestAmount ++ ")\n" ++
     "Most expensive time: " ++ highestTimeRaw ++ " ($" ++ printf "%.2f" (snd $ maximumBy (compare `on` snd) byTime) ++ ")\n"

-- Format the results for all weekends
formatResults :: [(String, [Expense])] -> String
formatResults weekends =
  let weekendAnalyses = map formatWeekendAnalysis weekends
      totalAllWeekends = sum $ map (totalSpending . snd) weekends
      allExpenses = concatMap snd weekends
      
      overallSummary = "Overall Summary\n" ++
                       "Total spending across all weekends: $" ++ printf "%.2f" totalAllWeekends ++ "\n" ++
                       "Average weekend spending: $" ++ printf "%.2f" (totalAllWeekends / fromIntegral (length weekends)) ++ "\n" ++
                       "Most expensive weekend: " ++ fst (maximumBy (compare `on` (totalSpending . snd)) weekends) ++ " ($" ++ 
                       printf "%.2f" (totalSpending . snd $ maximumBy (compare `on` (totalSpending . snd)) weekends) ++ ")\n"
                       
  in overallSummary ++ "\n" ++ intercalate "\n" weekendAnalyses

-- Main function
main :: IO ()
main = do
  content <- readFile "input.txt"
  let expenses = parseExpenses content
  let weekends = groupByWeekend expenses
  putStrLn (formatResults weekends)