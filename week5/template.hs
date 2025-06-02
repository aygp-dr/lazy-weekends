-- Week 5: Monads and IO
-- Complete the function implementations below

import System.IO
import Data.List
import Control.Monad
import Control.Monad.State
import System.Exit

-- Data types
data ReadingStatus = Unread | InProgress | Completed
  deriving (Eq)

data Book = Book
  { title :: String
  , author :: String
  , status :: ReadingStatus
  }

type ReadingList = [Book]

-- Show instances
instance Show ReadingStatus where
  show status = undefined -- TODO: Implement show for ReadingStatus

instance Show Book where
  show book = undefined -- TODO: Implement show for Book

-- Parse functions
parseStatus :: String -> ReadingStatus
parseStatus str = undefined -- TODO: Implement parsing for ReadingStatus

parseBook :: String -> Book
parseBook line = undefined -- TODO: Implement parsing for Book

-- File IO functions
readBookList :: FilePath -> IO ReadingList
readBookList filePath = undefined -- TODO: Implement file reading

saveBookList :: FilePath -> ReadingList -> IO ()
saveBookList filePath books = undefined -- TODO: Implement file saving

-- Book management functions
addBook :: String -> String -> ReadingStatus -> ReadingList -> ReadingList
addBook title author status books = undefined -- TODO: Implement book addition

markBookAsRead :: Int -> ReadingList -> ReadingList
markBookAsRead index books = undefined -- TODO: Implement status update

-- UI functions
displayMenu :: IO ()
displayMenu = do
  putStrLn "\nWeekend Reading Tracker"
  putStrLn "1. View Reading List"
  putStrLn "2. Add New Book"
  putStrLn "3. Mark Book as Read"
  putStrLn "4. Save and Exit"
  putStr "Choose an option: "
  hFlush stdout

displayBooks :: ReadingList -> IO ()
displayBooks books = undefined -- TODO: Implement book display

getBookDetails :: IO (String, String, ReadingStatus)
getBookDetails = undefined -- TODO: Implement input collection

-- Main program loop using the State monad
type AppState = StateT ReadingList IO

runApp :: FilePath -> AppState ()
runApp filePath = do
  displayBooks' -- Display initial books
  processCommands filePath
  where
    displayBooks' = do
      books <- get
      lift $ displayBooks books

processCommands :: FilePath -> AppState ()
processCommands filePath = undefined -- TODO: Implement command processing

-- Main function
main :: IO ()
main = do
  let filePath = "input.txt"
  books <- readBookList filePath
  evalStateT (runApp filePath) books