-- Week 3: Pattern Matching and Custom Types
-- Complete the type definitions and function implementations below

import System.IO
import Data.List
import Data.Maybe

-- Define custom types for Priority, Status, and Task
data Priority = -- TODO: Define priority levels (High, Medium, Low)

data Status = -- TODO: Define status types (NotStarted, InProgress, Completed)

data Task = -- TODO: Define Task type with description, priority, and status

-- Show instances for custom types
instance Show Priority where
  show = undefined -- TODO: Implement show for Priority

instance Show Status where
  show = undefined -- TODO: Implement show for Status

instance Show Task where
  show = undefined -- TODO: Implement show for Task

-- Parse Priority from string
parsePriority :: String -> Priority
parsePriority = undefined -- TODO: Implement parsing logic

-- Parse Status from string
parseStatus :: String -> Status
parseStatus = undefined -- TODO: Implement parsing logic

-- Parse a single line into a Task
parseTask :: String -> Task
parseTask = undefined -- TODO: Implement parsing logic

-- Parse all tasks from input content
parseTasks :: String -> [Task]
parseTasks = undefined -- TODO: Implement parsing logic

-- Filter tasks by priority
tasksByPriority :: Priority -> [Task] -> [Task]
tasksByPriority = undefined -- TODO: Implement filtering logic

-- Filter tasks by status
tasksByStatus :: Status -> [Task] -> [Task]
tasksByStatus = undefined -- TODO: Implement filtering logic

-- Calculate completion percentage for a list of tasks
completionPercentage :: [Task] -> Double
completionPercentage = undefined -- TODO: Implement calculation

-- Get the next tasks to work on (not completed, sorted by priority)
nextTasks :: [Task] -> [Task]
nextTasks = undefined -- TODO: Implement task prioritization

-- Format the results into a readable string
formatResults :: [Task] -> String
formatResults = undefined -- TODO: Implement formatting logic

-- Main function
main :: IO ()
main = do
  content <- readFile "input.txt"
  let tasks = parseTasks content
  putStrLn (formatResults tasks)