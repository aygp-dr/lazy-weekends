-- Week 5: Solution
-- A complete implementation of the Week 5 exercise

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
  show Unread = "Unread"
  show InProgress = "In Progress"
  show Completed = "Completed"

instance Show Book where
  show (Book title author status) =
    "\"" ++ title ++ "\" by " ++ author ++ " (" ++ show status ++ ")"

-- Parse functions
parseStatus :: String -> ReadingStatus
parseStatus "Unread" = Unread
parseStatus "In Progress" = InProgress
parseStatus "InProgress" = InProgress
parseStatus "Completed" = Completed
parseStatus _ = Unread  -- Default to Unread for invalid inputs

parseBook :: String -> Book
parseBook line =
  let parts = splitOn '|' line
      bookTitle = parts !! 0
      bookAuthor = parts !! 1
      bookStatus = parseStatus (parts !! 2)
  in Book bookTitle bookAuthor bookStatus

-- Helper function to split a string by delimiter
splitOn :: Char -> String -> [String]
splitOn delimiter = foldr f [[]]
  where
    f c acc@(x:xs)
      | c == delimiter = []:acc
      | otherwise = (c:x):xs

-- File IO functions
readBookList :: FilePath -> IO ReadingList
readBookList filePath = do
  content <- readFile filePath
  return $ map parseBook (lines content)

saveBookList :: FilePath -> ReadingList -> IO ()
saveBookList filePath books = do
  let bookLines = map formatBookForFile books
  writeFile filePath (unlines bookLines)
  where
    formatBookForFile (Book t a s) = t ++ "|" ++ a ++ "|" ++ show s

-- Book management functions
addBook :: String -> String -> ReadingStatus -> ReadingList -> ReadingList
addBook title author status books = Book title author status : books

markBookAsRead :: Int -> ReadingList -> ReadingList
markBookAsRead index books
  | index < 1 || index > length books = books
  | otherwise = 
      take (index-1) books ++ 
      [updateStatus (books !! (index-1))] ++ 
      drop index books
  where
    updateStatus (Book t a _) = Book t a Completed

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
displayBooks [] = putStrLn "Your reading list is empty."
displayBooks books = do
  putStrLn "\nCurrent Reading List:"
  forM_ (zip [1..] books) $ \(i, book) ->
    putStrLn $ show i ++ ". " ++ show book

getBookDetails :: IO (String, String, ReadingStatus)
getBookDetails = do
  putStr "Enter book title: "
  hFlush stdout
  title <- getLine
  
  putStr "Enter author: "
  hFlush stdout
  author <- getLine
  
  putStr "Enter status (Unread/In Progress/Completed): "
  hFlush stdout
  statusStr <- getLine
  let status = parseStatus statusStr
  
  return (title, author, status)

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
processCommands filePath = do
  lift displayMenu
  choice <- lift getLine
  
  case choice of
    "1" -> do  -- View Reading List
      books <- get
      lift $ displayBooks books
      processCommands filePath
      
    "2" -> do  -- Add New Book
      (title, author, status) <- lift getBookDetails
      modify $ addBook title author status
      lift $ putStrLn "Book added successfully!"
      processCommands filePath
      
    "3" -> do  -- Mark Book as Read
      lift $ putStr "Enter book number to mark as read: "
      lift $ hFlush stdout
      indexStr <- lift getLine
      let index = read indexStr :: Int
      books <- get
      if index > 0 && index <= length books
        then do
          let book = books !! (index - 1)
          modify $ markBookAsRead index
          lift $ putStrLn $ "Book \"" ++ title book ++ "\" marked as Completed"
        else lift $ putStrLn "Invalid book number!"
      processCommands filePath
      
    "4" -> do  -- Save and Exit
      books <- get
      lift $ putStr "Saving reading list... "
      lift $ saveBookList filePath books
      lift $ putStrLn "Done!"
      lift $ putStrLn "Goodbye!"
      
    _ -> do    -- Invalid choice
      lift $ putStrLn "Invalid option. Please try again."
      processCommands filePath

-- Main function
main :: IO ()
main = do
  let filePath = "input.txt"
  books <- readBookList filePath
  evalStateT (runApp filePath) books