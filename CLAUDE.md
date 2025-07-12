# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Lazy Weekends is an 8-week self-paced Haskell tutorial designed for busy developers. Each week contains a structured problem with input parsing requirements, starter templates, and reference solutions.

## Development Commands

### Build and Run
```bash
# Check dependencies (Haskell toolchain)
make deps

# Run a specific week's solution
make spoilers WEEK=week1

# Run all solutions
make spoilers WEEK=all

# Clean build artifacts
make clean
```

### Linting and Code Quality
```bash
# Run all linters (recommended before commits)
make lint

# Individual linters
make lint-haskell  # HLint + GHC checks
make lint-org      # Org-mode files
make lint-sh       # Shell scripts
make lint-make     # Makefile
```

### Working with Weekly Exercises
```bash
# Navigate to a week
cd week1/

# Run template interactively
ghci template.hs

# Compile and run solution
ghc -o solution spoilers/solution.hs && ./solution
```

## Code Architecture

### Weekly Structure (weeks 1-7)
- `README.md` - Problem description and learning goals
- `input.txt` - Structured input data (AOC-style)
- `template.hs` - Starter code with type signatures
- `spoilers/solution.hs` - Reference implementation

### Week 8 - Modular Project
```
week8/
├── src/
│   └── WeekendPlanner/
│       ├── Schedule.hs  # Scheduling algorithms
│       ├── Storage.hs   # Data persistence
│       └── Types.hs     # Core data types
└── data.txt             # Project data storage
```

## Haskell Development Guidelines

### When implementing solutions:
1. Start with type signatures - they guide the implementation
2. Parse input files line by line using standard IO functions
3. Use pattern matching and recursion as primary tools
4. Leverage list comprehensions for data transformations
5. Week 8 introduces modular design - separate concerns into modules

### Common patterns in this codebase:
```haskell
-- Input parsing pattern
main = do
    contents <- readFile "input.txt"
    let lines = lines contents
    -- Process lines...

-- Type-first development
processData :: [String] -> Result
processData = undefined  -- Fill in implementation
```

## Testing Approach

No formal test framework is used. Verification is done by:
1. Running solutions against provided input files
2. Comparing output with expected results in problem descriptions
3. Using GHCi for interactive testing of individual functions

## Important Notes

- This is an educational project - prioritize clarity over optimization
- Solutions should handle the specific input format for each week
- The project uses direct GHC compilation, not Stack or Cabal
- Week problems increase in complexity - don't skip ahead
- Reference solutions are provided but try solving independently first