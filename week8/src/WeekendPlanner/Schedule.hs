-- | Scheduling algorithms for the Weekend Planner application
module WeekendPlanner.Schedule
  ( -- * Scheduling Functions
    generateSchedule
  , ScheduleItem(..)
  , Schedule
  ) where

import Data.List (sortOn)
import Data.Ord (Down(..))
import qualified Data.Set as S
import qualified Data.Map as M

import WeekendPlanner.Types

-- | A scheduled project with its allocated time
data ScheduleItem = ScheduleItem
  { scheduleProject :: Project
  , allocatedHours  :: Hours
  } deriving (Eq, Show)

-- | A complete schedule
type Schedule = [ScheduleItem]

-- | Generate an optimal schedule based on available hours
generateSchedule :: [Project] -> Hours -> Schedule
generateSchedule projects availableHours =
  let (schedule, _) = scheduleProjects sortedProjects availableHours M.empty
  in schedule
  where
    -- Sort projects by priority (high to low) and then by estimated hours (low to high)
    sortedProjects = sortOn (\p -> (Down (projectPriority p), projectHours p)) 
                    $ filter (\p -> projectStatus p /= Completed) projects

-- | Recursively build a schedule respecting dependencies
scheduleProjects :: [Project] -> Hours -> M.Map String Bool -> (Schedule, Hours)
scheduleProjects [] remainingHours _ = ([], remainingHours)
scheduleProjects (p:ps) remainingHours completed
  | remainingHours <= 0 = ([], remainingHours)  -- No time left
  | not (canSchedule p completed) =
      -- Can't schedule this project yet, try the next one
      let (schedule, leftHours) = scheduleProjects ps remainingHours completed
      in (schedule, leftHours)
  | otherwise =
      -- Can schedule this project
      let hoursToAllocate = min remainingHours (projectHours p)
          scheduleItem = ScheduleItem p hoursToAllocate
          newRemaining = remainingHours - hoursToAllocate
          newCompleted = M.insert (projectName p) True completed
          (restSchedule, finalRemaining) = scheduleProjects ps newRemaining newCompleted
      in (scheduleItem : restSchedule, finalRemaining)

-- | Check if a project can be scheduled based on its dependencies
canSchedule :: Project -> M.Map String Bool -> Bool
canSchedule project completed =
  all (\dep -> M.findWithDefault False dep completed) (projectDeps project)