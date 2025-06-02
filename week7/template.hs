-- Week 7: Performance and Optimization
-- This implementation works but needs optimization for larger inputs

import System.IO
import Data.List
import Data.Maybe
import Text.Printf
import System.Time.Extra
import qualified Data.Map as M

-- Data types
data LocationType = Base | Nature | Beach | Cultural | Food | Entertainment | Relaxation
  deriving (Show, Eq)

data Location = Location
  { locName :: String
  , locType :: LocationType
  , region :: String
  , rating :: Double
  , cost :: Int
  } deriving (Show, Eq)

data Distance = Distance
  { from :: String
  , to :: String
  , miles :: Int
  , hours :: Double
  } deriving (Show, Eq)

type Route = [String]
type TripPlan = (Route, Int, Double) -- (route, totalCost, totalTime)

-- Parsing functions
parseLocationType :: String -> LocationType
parseLocationType "Base" = Base
parseLocationType "Nature" = Nature
parseLocationType "Beach" = Beach
parseLocationType "Cultural" = Cultural
parseLocationType "Food" = Food
parseLocationType "Entertainment" = Entertainment
parseLocationType "Relaxation" = Relaxation
parseLocationType _ = error "Invalid location type"

parseLocation :: String -> Location
parseLocation line =
  let parts = splitOn ',' line
      name = parts !! 0
      locType = parseLocationType (parts !! 1)
      reg = parts !! 2
      rat = read (parts !! 3) :: Double
      cst = read (parts !! 4) :: Int
  in Location name locType reg rat cst

parseDistance :: String -> Distance
parseDistance line =
  let parts = splitOn ',' line
      fromLoc = parts !! 0
      toLoc = parts !! 1
      dist = read (parts !! 2) :: Int
      time = read (parts !! 3) :: Double
  in Distance fromLoc toLoc dist time

-- Helper function to split a string by delimiter
splitOn :: Char -> String -> [String]
splitOn delimiter = foldr f [[]]
  where
    f c acc@(x:xs)
      | c == delimiter = []:acc
      | otherwise = (c:x):xs

-- Parse input file
parseInput :: String -> ([Location], [Distance])
parseInput content =
  let lines' = filter (not . isPrefixOf "#") $ lines content
      locationStart = fromMaybe 0 $ findIndex (== "# LOCATIONS") lines'
      distanceStart = fromMaybe 0 $ findIndex (== "# DISTANCES") lines'
      locationLines = takeWhile (/= "# DISTANCES") $ drop (locationStart + 1) lines'
      distanceLines = drop (distanceStart + 1) lines'
      locations = map parseLocation $ filter (not . null) locationLines
      distances = map parseDistance $ filter (not . null) distanceLines
  in (locations, distances)

-- Find distances between locations
getDistance :: [Distance] -> String -> String -> Maybe Distance
getDistance distances from to =
  find (\d -> (from == from d && to == to d) || (from == to d && to == from d)) distances

-- Calculate total distance of a route
routeDistance :: [Distance] -> Route -> Int
routeDistance _ [] = 0
routeDistance _ [_] = 0
routeDistance distances (loc1:loc2:rest) =
  let dist = getDistance distances loc1 loc2
  in case dist of
       Just d -> miles d + routeDistance distances (loc2:rest)
       Nothing -> 0 + routeDistance distances (loc2:rest)

-- Calculate total time of a route
routeTime :: [Distance] -> Route -> Double
routeTime _ [] = 0
routeTime _ [_] = 0
routeTime distances (loc1:loc2:rest) =
  let dist = getDistance distances loc1 loc2
  in case dist of
       Just d -> hours d + routeTime distances (loc2:rest)
       Nothing -> 0 + routeTime distances (loc2:rest)

-- Calculate total cost of a route
routeCost :: [Location] -> Route -> Int
routeCost locations route =
  sum [cost loc | loc <- locations, locName loc `elem` route, locName loc /= "Home"]

-- Generate all possible routes of a given length
-- This is the function that needs optimization!
generateRoutes :: [Location] -> [Distance] -> Int -> [Route]
generateRoutes locations distances length' =
  let home = "Home"
      validDestinations = filter (\loc -> locName loc /= home) locations
      destinationNames = map locName validDestinations
      
      -- Generate all permutations (very inefficient)
      allPerms = permutations destinationNames
      
      -- Filter to routes of the right length
      routesOfLength = filter (\r -> length r == length') allPerms
      
      -- Add home as start and end points
      completeRoutes = map (\r -> home : r ++ [home]) routesOfLength
      
      -- Filter to only include routes where all segments are connected
      isConnected route =
        all (\(a, b) -> isJust $ getDistance distances a b) $ zip route (tail route)
        
  in filter isConnected completeRoutes

-- Find the optimal route based on criteria
findOptimalRoute :: [Location] -> [Distance] -> Int -> TripPlan
findOptimalRoute locations distances days =
  let routes = generateRoutes locations distances days
      
      -- Calculate metrics for each route
      routeMetrics route = 
        let totalDist = routeDistance distances route
            totalTime = routeTime distances route
            totalCost = routeCost locations route
            -- Simple score: lower is better
            score = totalDist + totalCost * 2 + floor (totalTime * 60)
        in (route, totalCost, totalTime, score)
      
      allMetrics = map routeMetrics routes
      
      -- Find the route with the lowest score
      optimalRoute = minimumBy (\(_, _, _, s1) (_, _, _, s2) -> compare s1 s2) allMetrics
      
  in let (route, cost, time, _) = optimalRoute
     in (route, cost, time)

-- Format the trip plan
formatTripPlan :: [Location] -> TripPlan -> String
formatTripPlan locations (route, cost, time) =
  let dayByDay = zipWith formatDay [0..] route
      formatDay 0 loc = "Start: " ++ loc
      formatDay i loc | i == length route - 1 = "Return: " ++ loc
      formatDay i loc =
        let locationInfo = find (\l -> locName l == loc) locations
            costStr = case locationInfo of
                        Just l -> " (cost: $" ++ show (cost l) ++ ")"
                        Nothing -> ""
        in "Day " ++ show i ++ ": " ++ loc ++ costStr
      
      totalDistance = routeDistance (error "Not used here") route
      
      result = "Optimal weekend trip:\n" ++
               unlines dayByDay ++ "\n" ++
               "Total distance: " ++ show totalDistance ++ " miles\n" ++
               "Total cost: $" ++ show cost ++ "\n" ++
               "Estimated time: " ++ printf "%.1f" time ++ " hours driving"
               
  in result

-- Main function
main :: IO ()
main = do
  content <- readFile "input.txt"
  let (locations, distances) = parseInput content
  
  putStrLn "Trip Planning Results"
  
  start <- getMonotonicTime
  let optimalRoute = findOptimalRoute locations distances 2  -- Plan a 2-day trip
  end <- getMonotonicTime
  
  putStrLn $ "Planning time: " ++ printf "%.3f" (end - start) ++ " seconds\n"
  putStrLn $ formatTripPlan locations optimalRoute