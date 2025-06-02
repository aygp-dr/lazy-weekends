# Week 3: Pattern Matching and Custom Types

## Learning Goals
- Create and use custom data types
- Master pattern matching for data extraction
- Use algebraic data types to model domain concepts

## Problem Description
This week, you'll build a weekend project planner that manages tasks with different priorities and statuses. Your program will organize tasks, track their completion status, and help prioritize the most important ones.

The input data contains task descriptions with priorities (High, Medium, Low) and current statuses (NotStarted, InProgress, Completed).

## Tasks
1. Define custom types for Task, Priority, and Status
2. Parse the input data into your custom types
3. Implement functions to filter and sort tasks
4. Calculate completion statistics
5. Format and output a task summary

## Tips
- Use algebraic data types (ADTs) with constructors for Priority and Status
- Pattern matching is particularly powerful with custom types
- Consider using record syntax for the Task type
- Practice exhaustive pattern matching to handle all cases

## Example
Given input like:
```
Paint bedroom wall|High|InProgress
Read programming book|Medium|NotStarted
Water plants|Low|Completed
```

Your program might output:
```
Weekend Tasks Summary:
High priority tasks: 1 (0% complete)
Medium priority tasks: 1 (0% complete)
Low priority tasks: 1 (100% complete)
Overall completion: 33%

Next up:
1. Paint bedroom wall (High, InProgress)
2. Read programming book (Medium, NotStarted)
```

## Getting Started
The template.hs file provides the structure with type definitions and function signatures. Implement each function and test with the provided input.

This week will really strengthen your understanding of Haskell's type system!