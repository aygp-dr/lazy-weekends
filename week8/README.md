# Week 8: Real-world Application

## Learning Goals
- Build a complete, practical Haskell application
- Combine all the concepts from previous weeks
- Handle command-line arguments and configuration
- Create a well-structured, modular codebase

## Problem Description
For your final project, you'll build a complete weekend project management tool that helps users plan and track their weekend projects. This application will allow users to:

1. Add new weekend projects with estimated time, priority, and dependencies
2. Track progress on ongoing projects
3. Generate optimal schedules based on available time and priorities
4. Export reports on completed projects

## Tasks
1. Design a modular architecture with separate modules for core functionality
2. Implement command-line argument parsing
3. Create a data persistence layer (using files)
4. Build scheduling algorithms that respect dependencies
5. Generate reports and visualizations

## Tips
- Use the `optparse-applicative` library for command-line parsing
- Create a clean module structure with clear responsibilities
- Apply techniques from previous weeks: custom types, monads, efficient algorithms
- Add proper error handling and user-friendly messages
- Consider making your application configurable

## Example Usage
```
$ weekend-planner add "Build bookshelf" --hours 4 --priority high
Project added successfully!

$ weekend-planner add "Plant garden" --hours 3 --priority medium --depends-on "Buy seeds"
Project added successfully!

$ weekend-planner list
Weekend Projects:
1. Build bookshelf (High, 4h, No dependencies)
2. Plant garden (Medium, 3h, Depends on: Buy seeds)

$ weekend-planner schedule --available-hours 6
Optimal Schedule for this Weekend:
1. Build bookshelf (4h)
2. Buy seeds (1h)
Total: 5h (1h remaining)

$ weekend-planner complete "Build bookshelf"
Project marked as complete!

$ weekend-planner report --month "June"
June Weekend Projects:
Completed: 3 projects (10 hours)
Pending: 2 projects (7 hours)
Efficiency: 85% (time estimates vs. actual)
```

## Getting Started
For this final week, you'll create multiple Haskell files in a proper project structure. Start by thinking about your module design, then implement each component step by step.

This project brings together everything you've learned and results in a practical tool you might actually use!