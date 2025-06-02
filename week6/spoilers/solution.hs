-- Week 6: Solution
-- A complete implementation of the Week 6 exercise

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Char as C
import qualified Text.Parsec.Combinator as C
import Data.List
import Data.Ord
import System.IO
import Text.Printf

-- Data types
data Movie = Movie
  { title :: String
  , year :: Int
  , director :: String
  , genres :: [String]
  , watchDate :: String
  , rating :: Double
  , notes :: String
  }

instance Show Movie where
  show movie = 
    title movie ++ " (" ++ show (year movie) ++ ") - " ++ 
    printf "%.1f" (rating movie) ++ "/5"

-- Parsers
-- Helper parsers
whitespace :: Parser String
whitespace = many (oneOf " \t\n\r")

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

-- Specific field parsers
movieTitleParser :: Parser (String, Int)
movieTitleParser = do
  string "MOVIE:" >> whitespace
  title <- manyTill anyChar (try (string " ("))
  year <- many1 digit
  string ")"
  return (title, read year)

directorParser :: Parser String
directorParser = do
  string "DIRECTOR:" >> whitespace
  manyTill anyChar newline

genresParser :: Parser [String]
genresParser = do
  string "GENRES:" >> whitespace
  genres <- sepBy (many1 (noneOf ",\n\r")) (string ", ")
  newline
  return genres

watchDateParser :: Parser String
watchDateParser = do
  string "WATCHED:" >> whitespace
  date <- many1 (digit <|> char '-')
  newline
  return date

ratingParser :: Parser Double
ratingParser = do
  string "RATING:" >> whitespace
  rating <- many1 (digit <|> char '.')
  string "/5"
  newline
  return (read rating)

notesParser :: Parser String
notesParser = do
  string "NOTES:" >> whitespace
  notes <- manyTill anyChar (try newline)
  return notes

-- Complete movie parser
movieParser :: Parser Movie
movieParser = do
  whitespace
  (movieTitle, movieYear) <- movieTitleParser
  newline
  movieDirector <- directorParser
  movieGenres <- genresParser
  movieWatchDate <- watchDateParser
  movieRating <- ratingParser
  movieNotes <- notesParser
  optional (many1 newline)  -- Handle optional blank lines between movies
  return $ Movie movieTitle movieYear movieDirector movieGenres movieWatchDate movieRating movieNotes

-- Parse all movies from input
moviesParser :: Parser [Movie]
moviesParser = many1 movieParser <* eof

-- Analysis functions
favoriteGenres :: [Movie] -> [(String, Int)]
favoriteGenres movies = 
  let allGenres = concatMap genres movies
      uniqueGenres = nub allGenres
      genreCount genre = (genre, length $ filter (== genre) allGenres)
  in sortBy (comparing (negate . snd)) $ map genreCount uniqueGenres

averageRating :: [Movie] -> Double
averageRating movies = 
  let totalRating = sum $ map rating movies
      count = length movies
  in if count > 0 then totalRating / fromIntegral count else 0.0

highestRatedMovie :: [Movie] -> Movie
highestRatedMovie = maximumBy (comparing rating)

generateRecommendations :: [Movie] -> [String]
generateRecommendations movies =
  let topMovie = highestRatedMovie movies
      topDirector = director topMovie
      (topGenre, _) = head $ favoriteGenres movies
      directorRec = "More films by " ++ topDirector
      genreRec = "More " ++ topGenre ++ " films"
      ratingRec = "Films rated above " ++ printf "%.1f" (averageRating movies) ++ "/5"
  in [directorRec, genreRec, ratingRec]

-- Format the results
formatAnalysis :: [Movie] -> String
formatAnalysis movies =
  let genreCounts = take 3 $ favoriteGenres movies
      formatGenre (g, c) = g ++ " (" ++ show c ++ ")"
      genreList = intercalate ", " $ map formatGenre genreCounts
      
      avg = averageRating movies
      top = highestRatedMovie movies
      recs = generateRecommendations movies
      
  in "Movie Watchlist Analysis\n" ++
     "Total movies watched: " ++ show (length movies) ++ "\n" ++
     "Favorite genres: " ++ genreList ++ "\n" ++
     "Average rating: " ++ printf "%.2f" avg ++ "/5\n" ++
     "Highest rated: " ++ show top ++ "\n\n" ++
     "Recommendations:\n" ++ unlines (map ("- " ++) recs)

-- Main function
main :: IO ()
main = do
  content <- readFile "input.txt"
  case parse moviesParser "Movie Watchlist" content of
    Left err -> putStrLn $ "Error parsing input: " ++ show err
    Right movies -> putStrLn $ formatAnalysis movies