# Week 4: Recursion and Higher-Order Functions

## Learning Goals
- Master recursion for solving complex problems
- Use higher-order functions effectively
- Understand folds and their applications

## Problem Description
This week, you'll build a weekend budget tracker that analyzes your spending patterns. The input data contains records of weekend expenses with categories, amounts, and timestamps.

Your task is to process this data using recursion and higher-order functions to:
- Calculate total spending by category
- Find patterns in your spending
- Identify the most expensive weekends

## Tasks
1. Parse the expense data
2. Implement recursive functions to analyze spending patterns
3. Use higher-order functions to transform and filter data
4. Implement fold operations to aggregate expenses
5. Format and present your financial analysis

## Tips
- Try using recursion for traversing nested data structures
- Practice with `foldr`, `foldl`, and `foldl'` to understand their differences
- Higher-order functions like `map`, `filter`, and `zipWith` can simplify complex operations
- Composition (`(.)`) can help chain functions together elegantly

## Example
Given input like:
```
2023-05-06,Coffee,4.50,Morning
2023-05-06,Groceries,65.20,Afternoon
2023-05-07,Movie,15.00,Evening
2023-05-07,Dinner,35.80,Evening
```

Your analysis might show:
```
Weekend of 2023-05-06/07
Total spending: $120.50
By category:
  Food & Drink: $105.50
  Entertainment: $15.00
Highest expense: Groceries ($65.20)
Most expensive time: Afternoon ($65.20)
```

## Getting Started
The template.hs file contains the structure and type signatures. Fill in the implementations using recursion and higher-order functions where appropriate.

This week will really strengthen your functional programming skills!