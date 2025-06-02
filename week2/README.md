# Week 2: Lists and List Comprehensions

## Learning Goals
- Master list operations and transformations
- Use list comprehensions effectively
- Improve parsing skills with more complex data

## Problem Description
This week, you'll analyze a schedule of weekend activities across multiple days. The input contains activities for Saturday and Sunday, each with a time slot and category.

Your task is to organize and analyze this weekend schedule, answering questions like:
- Which day has more activities?
- How much time is spent on each category of activity?
- What's the busiest time of the weekend?

## Tasks
1. Parse the structured input data
2. Group activities by day and by category
3. Calculate time spent per category
4. Find the busiest hour of the weekend
5. Format and present your analysis

## Tips
- Use list comprehensions to filter and transform data
- Try using higher-order functions like `filter`, `map`, and `groupBy`
- Haskell's pattern matching can help with destructuring data

## Example
Given input like:
```
Saturday 09:00 Exercise Running
Saturday 11:30 Social Coffee
Sunday 10:00 Hobby Painting
Sunday 14:00 Chores Cleaning
```

Your analysis might show:
```
Saturday: 2 activities
Sunday: 2 activities
Time by category:
  Exercise: 1 hour
  Social: 1 hour
  Hobby: 2 hours
  Chores: 1 hour
Busiest time: Sunday 10:00-12:00
```

## Getting Started
The template.hs file contains the structure and type signatures. Fill in the implementations and test with the provided input.

Build on what you learned last week and explore new Haskell techniques!