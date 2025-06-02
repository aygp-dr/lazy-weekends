-- Week 3: Solution
-- A complete implementation of the Week 3 exercise

import System.IO
import Data.List
import Data.Maybe
import Text.Printf

-- Define custom types for Priority, Status, and Task
data Priority = High | Medium | Low
  deriving (Eq, Ord)

data Status = NotStarted | InProgress | Completed
  deriving (Eq)

data Task = Task 
  { description :: String
  , priority :: Priority
  , status :: Status
  }

-- Show instances for custom types
instance Show Priority where
  show High = "High"
  show Medium = "Medium"
  show Low = "Low"

instance Show Status where
  show NotStarted = "Not Started"
  show InProgress = "In Progress"
  show Completed = "Completed"

instance Show Task where
  show (Task desc prio stat) = 
    desc ++ " (" ++ show prio ++ ", " ++ show stat ++ ")"

-- Parse Priority from string
parsePriority :: String -> Priority
parsePriority "High" = High
parsePriority "Medium" = Medium
parsePriority "Low" = Low
parsePriority _ = error "Invalid priority"

-- Parse Status from string
parseStatus :: String -> Status
parseStatus "NotStarted" = NotStarted
parseStatus "InProgress" = InProgress
parseStatus "Completed" = Completed
parseStatus _ = error "Invalid status"

-- Parse a single line into a Task
parseTask :: String -> Task
parseTask line =
  let parts = splitOn '|' line
      desc = parts !! 0
      prio = parsePriority (parts !! 1)
      stat = parseStatus (parts !! 2)
  in Task desc prio stat

-- Helper function to split a string by delimiter
splitOn :: Char -> String -> [String]
splitOn delimiter = foldr f [[]]
  where
    f c acc@(x:xs)
      | c == delimiter = []:acc
      | otherwise = (c:x):xs

-- Parse all tasks from input content
parseTasks :: String -> [Task]
parseTasks content = map parseTask (lines content)

-- Filter tasks by priority
tasksByPriority :: Priority -> [Task] -> [Task]
tasksByPriority p = filter (\task -> priority task == p)

-- Filter tasks by status
tasksByStatus :: Status -> [Task] -> [Task]
tasksByStatus s = filter (\task -> status task == s)

-- Calculate completion percentage for a list of tasks
completionPercentage :: [Task] -> Double
completionPercentage tasks =
  let completed = length $ tasksByStatus Completed tasks
      total = length tasks
  in if total == 0 then 0.0 else (fromIntegral completed / fromIntegral total) * 100.0

-- Get the next tasks to work on (not completed, sorted by priority)
nextTasks :: [Task] -> [Task]
nextTasks tasks =
  let notCompleted = filter (\task -> status task /= Completed) tasks
  in sortOn priority notCompleted

-- Format the results into a readable string
formatResults :: [Task] -> String
formatResults tasks =
  let highTasks = tasksByPriority High tasks
      mediumTasks = tasksByPriority Medium tasks
      lowTasks = tasksByPriority Low tasks
      
      highCompletion = completionPercentage highTasks
      mediumCompletion = completionPercentage mediumTasks
      lowCompletion = completionPercentage lowTasks
      overallCompletion = completionPercentage tasks
      
      next = take 5 $ nextTasks tasks
      
      formatNext task n = show n ++ ". " ++ description task ++ " (" ++ show (priority task) ++ ", " ++ show (status task) ++ ")"
      nextTasksStr = unlines $ zipWith formatNext next [1..]
      
  in "Weekend Tasks Summary:\n" ++
     "High priority tasks: " ++ show (length highTasks) ++ " (" ++ printf "%.0f" highCompletion ++ "% complete)\n" ++
     "Medium priority tasks: " ++ show (length mediumTasks) ++ " (" ++ printf "%.0f" mediumCompletion ++ "% complete)\n" ++
     "Low priority tasks: " ++ show (length lowTasks) ++ " (" ++ printf "%.0f" lowCompletion ++ "% complete)\n" ++
     "Overall completion: " ++ printf "%.0f" overallCompletion ++ "%\n\n" ++
     "Next up:\n" ++ nextTasksStr

-- Main function
main :: IO ()
main = do
  content <- readFile "input.txt"
  let tasks = parseTasks content
  putStrLn (formatResults tasks)