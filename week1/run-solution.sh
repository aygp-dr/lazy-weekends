#!/bin/bash
# Simple script to run the Week 1 solution

echo "Running Week 1 solution..."

# Compile the solution
ghc -o solution spoilers/solution.hs

# Run the solution
./solution

# Clean up
rm -f solution *.hi *.o spoilers/*.hi spoilers/*.o

echo -e "\nWeek 1 solution completed!"
echo "Try modifying the input.txt file to test with different data."