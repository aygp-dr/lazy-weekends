# Week 1: Getting Started with Haskell

## Learning Goals
- Understand basic Haskell syntax
- Work with lists and simple functions
- Parse basic input data

## Problem Description
In this first challenge, you'll be analyzing a list of weekend activities and their durations. The goal is to find which activities take the most and least time, and calculate the total time spent on weekend activities.

Your input file contains a list of activities, one per line, with the name of the activity and the time it takes in minutes.

## Tasks
1. Parse the input file
2. Find the activity that takes the longest time
3. Find the activity that takes the shortest time
4. Calculate the total time spent on all activities
5. Format and output the results

## Tips
- Start by parsing each line into a tuple of (String, Int)
- Use Haskell's built-in functions like `maximum`, `minimum`, and `sum` to analyze the data
- Remember that Haskell is lazy, so you can chain operations efficiently

## Example
Given the input:
```
Reading 60
Cooking 90
Walking 45
```

Your program should output:
```
Longest activity: Cooking (90 minutes)
Shortest activity: Walking (45 minutes)
Total time: 195 minutes
```

## Getting Started
The template.hs file has the structure and type signatures to help you get started. Try to fill in the implementations and test your code with the provided input.

Good luck, and remember: take your time and enjoy the process!