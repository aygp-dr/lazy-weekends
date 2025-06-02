# Week 7: Performance and Optimization

## Learning Goals
- Understand Haskell's performance characteristics
- Use profiling tools to identify bottlenecks
- Apply optimization techniques to improve efficiency
- Work with larger datasets efficiently

## Problem Description
This week, you'll optimize a weekend trip planner that needs to process a large dataset of locations, distances, and costs to find optimal routes for weekend getaways. The initial implementation works but is too slow for larger inputs.

Your task is to profile the code, identify performance bottlenecks, and apply optimization techniques to make the program run efficiently.

## Tasks
1. Profile the initial implementation to identify bottlenecks
2. Apply optimization techniques (memoization, strictness, data structures)
3. Implement a more efficient algorithm for route finding
4. Compare performance metrics before and after optimization
5. Document your optimization process and results

## Tips
- Use GHC's profiling tools (`-prof`, `-fprof-auto`)
- Consider data structures with better access patterns (Map, Vector)
- Use strictness annotations (`!`) and BangPatterns where appropriate
- Look into memoization for expensive recursive computations
- Try different algorithms (greedy, dynamic programming) for route finding

## Example
Given a large input of locations and costs, your optimized program might produce:

```
Trip Planning Results
Before optimization: 15.2 seconds
After optimization: 0.3 seconds

Optimal weekend trip:
Start: Home
Day 1: Mountain View (cost: $120)
Day 2: Seaside (cost: $180)
Return: Home

Total distance: 320 miles
Total cost: $300
Estimated time: 6.5 hours driving
```

## Getting Started
The template.hs file contains the initial implementation that needs optimization. Run it with profiling enabled to identify bottlenecks before making changes.

Compile with profiling:
```bash
ghc -O2 -prof -fprof-auto -rtsopts template.hs
```

Run with profiling:
```bash
./template +RTS -p
```

This week will teach you how to write not just correct, but efficient Haskell code!