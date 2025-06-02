-- Week 4: Recursion and Higher-Order Functions
-- Complete the function implementations below

import System.IO
import Data.List
import Data.Maybe
import Text.Printf

-- Type definitions
type Date = String
type Category = String
type Amount = Double
type TimeOfDay = String
type Expense = (Date, Category, Amount, TimeOfDay)

-- Parse a single line into an Expense
parseExpense :: String -> Expense
parseExpense line = undefined -- TODO: Implement this function

-- Parse all expenses from input content
parseExpenses :: String -> [Expense]
parseExpenses content = undefined -- TODO: Implement this function

-- Group expenses by weekend (use recursion)
groupByWeekend :: [Expense] -> [(String, [Expense])]
groupByWeekend expenses = undefined -- TODO: Implement using recursion

-- Calculate total spending for a list of expenses (use fold)
totalSpending :: [Expense] -> Amount
totalSpending expenses = undefined -- TODO: Implement using fold

-- Group expenses by category and sum amounts (use higher-order functions)
spendingByCategory :: [Expense] -> [(Category, Amount)]
spendingByCategory expenses = undefined -- TODO: Implement using higher-order functions

-- Group expenses by time of day and sum amounts (use higher-order functions)
spendingByTimeOfDay :: [Expense] -> [(TimeOfDay, Amount)]
spendingByTimeOfDay expenses = undefined -- TODO: Implement using higher-order functions

-- Find the highest expense (use higher-order functions)
highestExpense :: [Expense] -> Expense
highestExpense expenses = undefined -- TODO: Implement using higher-order functions

-- Format the results for a single weekend
formatWeekendAnalysis :: (String, [Expense]) -> String
formatWeekendAnalysis (weekend, expenses) = undefined -- TODO: Implement this function

-- Format the results for all weekends
formatResults :: [(String, [Expense])] -> String
formatResults weekends = undefined -- TODO: Implement this function

-- Main function
main :: IO ()
main = do
  content <- readFile "input.txt"
  let expenses = parseExpenses content
  let weekends = groupByWeekend expenses
  putStrLn (formatResults weekends)