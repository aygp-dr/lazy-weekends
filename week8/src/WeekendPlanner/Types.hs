-- | Core data types for the Weekend Planner application
module WeekendPlanner.Types
  ( -- * Data Types
    Project(..)
  , Status(..)
  , Priority(..)
  , Dependency
    -- * Type Aliases
  , ProjectName
  , Hours
  , Date
    -- * Helper Functions
  , statusFromString
  , priorityFromString
  , dependenciesFromString
  , statusToString
  , priorityToString
  , dependenciesToString
  ) where

import Data.List (intercalate)
import Data.Time (Day)
import qualified Data.Time as Time

-- | Type alias for project names
type ProjectName = String

-- | Type alias for time estimates in hours
type Hours = Double

-- | Type alias for dates
type Date = Maybe Day

-- | Type alias for project dependencies
type Dependency = String

-- | Project status
data Status = Pending | InProgress | Completed
  deriving (Eq, Ord, Show)

-- | Project priority
data Priority = High | Medium | Low
  deriving (Eq, Ord, Show)

-- | Project data type
data Project = Project
  { projectName     :: !ProjectName
  , projectStatus   :: !Status
  , projectPriority :: !Priority
  , projectHours    :: !Hours
  , projectDeps     :: ![Dependency]
  , projectStart    :: !Date
  , projectEnd      :: !Date
  } deriving (Eq, Show)

-- | Convert a string to Status
statusFromString :: String -> Status
statusFromString "Pending"    = Pending
statusFromString "InProgress" = InProgress
statusFromString "Completed"  = Completed
statusFromString _            = Pending

-- | Convert Status to a string
statusToString :: Status -> String
statusToString Pending    = "Pending"
statusToString InProgress = "InProgress"
statusToString Completed  = "Completed"

-- | Convert a string to Priority
priorityFromString :: String -> Priority
priorityFromString "High"   = High
priorityFromString "Medium" = Medium
priorityFromString "Low"    = Low
priorityFromString _        = Medium

-- | Convert Priority to a string
priorityToString :: Priority -> String
priorityToString High   = "High"
priorityToString Medium = "Medium"
priorityToString Low    = "Low"

-- | Parse dependencies from a comma-separated string
dependenciesFromString :: String -> [Dependency]
dependenciesFromString "None" = []
dependenciesFromString str    = split ',' str

-- | Convert dependencies to a comma-separated string
dependenciesToString :: [Dependency] -> String
dependenciesToString [] = "None"
dependenciesToString ds = intercalate "," ds

-- | Helper function to split a string by a delimiter
split :: Char -> String -> [String]
split delimiter = foldr f [[]]
  where
    f c acc@(x:xs)
      | c == delimiter = []:acc
      | otherwise      = (c:x):xs