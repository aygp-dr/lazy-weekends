-- | Storage functions for the Weekend Planner application
module WeekendPlanner.Storage
  ( -- * Storage Functions
    readProjects
  , writeProjects
  , getDataFilePath
  ) where

import System.IO
import System.Directory (getHomeDirectory, createDirectoryIfMissing)
import System.FilePath ((</>))
import Data.Maybe (fromMaybe)
import Data.Time (Day)
import qualified Data.Time as Time
import Text.Read (readMaybe)

import WeekendPlanner.Types

-- | Get the path to the data file
getDataFilePath :: IO FilePath
getDataFilePath = do
  homeDir <- getHomeDirectory
  let appDir = homeDir </> ".weekend-planner"
  createDirectoryIfMissing True appDir
  return $ appDir </> "projects.txt"

-- | Read projects from the data file
readProjects :: FilePath -> IO [Project]
readProjects filePath = do
  fileExists <- doesFileExist filePath
  if fileExists
    then do
      content <- readFile filePath
      return $ map parseProjectLine $ filter (not . isComment) $ lines content
    else return []
  where
    isComment line = case line of
                       '#':_ -> True
                       _     -> False

-- | Write projects to the data file
writeProjects :: FilePath -> [Project] -> IO ()
writeProjects filePath projects = do
  let header = "# Weekend Project Data\n# Format: name|status|priority|hours|dependencies|start_date|end_date\n"
      content = header ++ unlines (map formatProject projects)
  writeFile filePath content

-- | Parse a line from the data file into a Project
parseProjectLine :: String -> Project
parseProjectLine line = 
  let fields = split '|' line
      name = fields !! 0
      status = statusFromString $ fields !! 1
      priority = priorityFromString $ fields !! 2
      hours = read $ fields !! 3
      deps = dependenciesFromString $ fields !! 4
      start = parseDate $ fields !! 5
      end = parseDate $ fields !! 6
  in Project name status priority hours deps start end

-- | Format a Project for writing to the data file
formatProject :: Project -> String
formatProject project =
  projectName project ++ "|" ++
  statusToString (projectStatus project) ++ "|" ++
  priorityToString (projectPriority project) ++ "|" ++
  show (projectHours project) ++ "|" ++
  dependenciesToString (projectDeps project) ++ "|" ++
  formatDate (projectStart project) ++ "|" ++
  formatDate (projectEnd project)

-- | Parse a date string to a Maybe Day
parseDate :: String -> Maybe Day
parseDate "None" = Nothing
parseDate str = 
  case Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%d" str of
    Just day -> Just day
    Nothing  -> Nothing

-- | Format a Maybe Day to a string
formatDate :: Maybe Day -> String
formatDate Nothing = "None"
formatDate (Just day) = Time.formatTime Time.defaultTimeLocale "%Y-%m-%d" day

-- | Helper function to check if a file exists
doesFileExist :: FilePath -> IO Bool
doesFileExist filePath = do
  result <- tryIOError $ openFile filePath ReadMode
  case result of
    Left _  -> return False
    Right h -> hClose h >> return True

-- | Helper function to handle IO errors
tryIOError :: IO a -> IO (Either IOError a)
tryIOError action = 
  (Right <$> action) `catch` (\e -> return $ Left e)

-- | Helper function to split a string by a delimiter
split :: Char -> String -> [String]
split delimiter = foldr f [[]]
  where
    f c acc@(x:xs)
      | c == delimiter = []:acc
      | otherwise      = (c:x):xs