-- Week 7: Solution
-- An optimized implementation of the Week 7 exercise

{-# LANGUAGE BangPatterns #-}

import System.IO
import Data.List
import Data.Maybe
import Text.Printf
import System.Time.Extra
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import Control.Monad.State

-- Data types
data LocationType = Base | Nature | Beach | Cultural | Food | Entertainment | Relaxation
  deriving (Show, Eq, Ord)

data Location = Location
  { locName :: !String
  , locType :: !LocationType
  , region :: !String
  , rating :: !Double
  , cost :: !Int
  } deriving (Show, Eq)

data Distance = Distance
  { from :: !String
  , to :: !String
  , miles :: !Int
  , hours :: !Double
  } deriving (Show, Eq)

type Route = [String]
type TripPlan = (Route, Int, Double) -- (route, totalCost, totalTime)

-- Optimized data structures
type LocationMap = M.Map String Location
type DistanceMap = M.Map (String, String) Distance
type ConnectionMap = M.Map String (S.Set String)

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
      !name = parts !! 0
      !locType = parseLocationType (parts !! 1)
      !reg = parts !! 2
      !rat = read (parts !! 3) :: Double
      !cst = read (parts !! 4) :: Int
  in Location name locType reg rat cst

parseDistance :: String -> Distance
parseDistance line =
  let parts = splitOn ',' line
      !fromLoc = parts !! 0
      !toLoc = parts !! 1
      !dist = read (parts !! 2) :: Int
      !time = read (parts !! 3) :: Double
  in Distance fromLoc toLoc dist time

-- Helper function to split a string by delimiter (optimized)
splitOn :: Char -> String -> [String]
splitOn delimiter = go []
  where
    go current [] = [reverse current]
    go current (c:cs)
      | c == delimiter = reverse current : go [] cs
      | otherwise = go (c:current) cs

-- Parse input file (optimized)
parseInput :: String -> ([Location], [Distance])
parseInput content =
  let relevantLines = filter (not . isPrefixOf "#") $ filter (not . null) $ lines content
      (locationLines, distanceLines) = span (not . isPrefixOf "Home,") relevantLines
      locations = map parseLocation locationLines
      distances = map parseDistance distanceLines
  in (locations, distances)

-- Build optimized data structures
buildLocationMap :: [Location] -> LocationMap
buildLocationMap = M.fromList . map (\loc -> (locName loc, loc))

buildDistanceMap :: [Distance] -> DistanceMap
buildDistanceMap distances =
  let normalPairs = map (\d -> ((from d, to d), d)) distances
      reversePairs = map (\d -> ((to d, from d), d)) distances
  in M.fromList (normalPairs ++ reversePairs)

buildConnectionMap :: [Distance] -> ConnectionMap
buildConnectionMap distances =
  let addConnection m (Distance from to _ _) =
        let updateFrom = M.insertWith S.union from (S.singleton to)
            updateTo = M.insertWith S.union to (S.singleton from)
        in updateTo (updateFrom m)
  in foldl' addConnection M.empty distances

-- Optimized distance lookup
getDistance :: DistanceMap -> String -> String -> Maybe Distance
getDistance distMap from to = M.lookup (from, to) distMap

-- Calculate total distance of a route (optimized)
routeDistance :: DistanceMap -> Route -> Int
routeDistance distMap route = 
  sum $ mapMaybe (\(a, b) -> fmap miles $ getDistance distMap a b) $ zip route (tail route)

-- Calculate total time of a route (optimized)
routeTime :: DistanceMap -> Route -> Double
routeTime distMap route = 
  sum $ mapMaybe (\(a, b) -> fmap hours $ getDistance distMap a b) $ zip route (tail route)

-- Calculate total cost of a route (optimized)
routeCost :: LocationMap -> Route -> Int
routeCost locMap route =
  sum [cost loc | name <- route, name /= "Home", 
                 Just loc <- [M.lookup name locMap]]

-- Generate feasible routes using a more efficient algorithm
-- Now using a greedy approach with backtracking
generateRoutes :: ConnectionMap -> LocationMap -> DistanceMap -> Int -> [Route]
generateRoutes connMap locMap distMap days =
  let home = "Home"
      destinations = M.keys $ M.delete home locMap
      maxRoutes = 1000  -- Limit the number of routes to consider
      
      -- Use a scoring function to prioritize promising routes
      scorePartialRoute :: Route -> Double
      scorePartialRoute route =
        let dist = sum $ mapMaybe (\(a, b) -> fmap (fromIntegral . miles) $ 
                                   getDistance distMap a b) $ zip route (tail route)
            costs = sum [maybe 0 (fromIntegral . cost) $ M.lookup loc locMap | 
                          loc <- route, loc /= home]
            avgRating = sum [maybe 0 rating $ M.lookup loc locMap | 
                             loc <- route, loc /= home] / 
                        fromIntegral (length route - 1)
        in dist + costs - (avgRating * 10)  -- Lower score is better
      
      -- Build routes using depth-first search with heuristics
      buildRoutes :: State (S.Set Route) ()
      buildRoutes = do
        let initialRoute = [home]
        expandRoute initialRoute
        
      expandRoute :: Route -> State (S.Set Route) ()
      expandRoute route
        | length route > days + 1 = do
            -- If we've visited enough places, add home and store the route
            let completeRoute = route ++ [home]
            if isValidRoute completeRoute 
              then modify (S.insert completeRoute)
              else return ()
        | otherwise = do
            -- Get all possible next locations
            let current = last route
                visited = S.fromList route
                connected = maybe S.empty id $ M.lookup current connMap
                candidates = S.toList $ S.difference connected visited
            
            -- Sort candidates by a heuristic score
            let withScores = [(nextLoc, scorePartialRoute (route ++ [nextLoc])) | 
                              nextLoc <- candidates]
                sortedCandidates = map fst $ sortOn snd withScores
            
            -- Recursively expand each candidate
            mapM_ (\next -> expandRoute (route ++ [next])) sortedCandidates
      
      isValidRoute :: Route -> Bool
      isValidRoute route =
        all (\(a, b) -> isJust $ getDistance distMap a b) $ zip route (tail route)
      
      -- Run the state monad to generate routes
      routes = S.toList $ execState buildRoutes S.empty
      
  in take maxRoutes routes

-- Find the optimal route (optimized)
findOptimalRoute :: LocationMap -> DistanceMap -> ConnectionMap -> Int -> TripPlan
findOptimalRoute locMap distMap connMap days = 
  let routes = generateRoutes connMap locMap distMap days
      
      -- Calculate metrics for each route (strict evaluation)
      routeMetrics route = 
        let !totalDist = routeDistance distMap route
            !totalTime = routeTime distMap route
            !totalCost = routeCost locMap route
            !score = totalDist + totalCost * 2 + floor (totalTime * 60)
        in (route, totalCost, totalTime, score)
      
      allMetrics = map routeMetrics routes
      
      -- Find the route with the lowest score
      optimalRoute = minimumBy (\(_, _, _, s1) (_, _, _, s2) -> compare s1 s2) allMetrics
      
  in let (route, cost, time, _) = optimalRoute
     in (route, cost, time)

-- Format the trip plan (unchanged)
formatTripPlan :: LocationMap -> TripPlan -> String
formatTripPlan locMap (route, cost, time) =
  let dayByDay = zipWith formatDay [0..] route
      formatDay 0 loc = "Start: " ++ loc
      formatDay i loc | i == length route - 1 = "Return: " ++ loc
      formatDay i loc =
        let locationInfo = M.lookup loc locMap
            costStr = case locationInfo of
                        Just l -> " (cost: $" ++ show (cost l) ++ ")"
                        Nothing -> ""
        in "Day " ++ show i ++ ": " ++ loc ++ costStr
      
      totalDistance = case route of
                        [] -> 0
                        [_] -> 0
                        _ -> sum $ mapMaybe (\(a, b) -> fmap miles $ M.lookup (a, b) 
                                            (error "Not used here")) $ zip route (tail route)
      
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
  
  -- Time the parsing and data structure building
  start1 <- getMonotonicTime
  let (locations, distances) = parseInput content
      !locMap = buildLocationMap locations
      !distMap = buildDistanceMap distances
      !connMap = buildConnectionMap distances
  end1 <- getMonotonicTime
  
  putStrLn "Trip Planning Results"
  putStrLn $ "Data preparation: " ++ printf "%.3f" (end1 - start1) ++ " seconds"
  
  -- Time the route generation and optimization
  start2 <- getMonotonicTime
  let !optimalRoute = findOptimalRoute locMap distMap connMap 2  -- Plan a 2-day trip
  end2 <- getMonotonicTime
  
  putStrLn $ "Route optimization: " ++ printf "%.3f" (end2 - start2) ++ " seconds"
  putStrLn $ "Total planning time: " ++ printf "%.3f" (end1 - start1 + end2 - start2) ++ " seconds\n"
  
  putStrLn $ formatTripPlan locMap optimalRoute