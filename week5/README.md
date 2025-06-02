# Week 5: Monads and IO

## Learning Goals
- Understand and use monads in Haskell
- Work with the IO monad for file operations
- Process real-world data using monadic operations

## Problem Description
This week, you'll build a weekend reading tracker that helps manage a reading list. The application will read book data from a file, allow users to add new books, mark books as read, and save the updated reading list back to the file.

Unlike previous weeks, this exercise involves real file I/O operations and user interaction through the command line.

## Tasks
1. Define data types for books and reading lists
2. Implement functions to read book data from a file
3. Create functions to add books and update their status
4. Build a simple command-line interface using the IO monad
5. Implement file saving functionality

## Tips
- Use the `do` notation for cleaner IO operations
- Try using the `Maybe` monad for handling potential failures
- Use the `State` monad (from Control.Monad.State) for managing the reading list state
- Remember that monads are just a design pattern for sequencing operations

## Example Interaction
```
Weekend Reading Tracker
1. View Reading List
2. Add New Book
3. Mark Book as Read
4. Save and Exit
Choose an option: 1

Current Reading List:
1. "Functional Programming in Haskell" by John Smith (Unread)
2. "The Art of Computer Programming" by Donald Knuth (In Progress)
3. "Learn You a Haskell" by Miran Lipovaca (Completed)

Choose an option: 2
Enter book title: Haskell Design Patterns
Enter author: Ryan Lemmer
Enter status (Unread/In Progress/Completed): Unread
Book added successfully!

Choose an option: 3
Enter book number to mark as read: 1
Book "Functional Programming in Haskell" marked as Completed

Choose an option: 4
Saving reading list... Done!
Goodbye!
```

## Getting Started
The template.hs file provides the structure and basic IO functions. Fill in the implementations and experiment with different monadic operations.

This week's exercise will give you practical experience with Haskell's IO system!