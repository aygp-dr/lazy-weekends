# Week 6: Parsing Libraries

## Learning Goals
- Use proper parsing libraries (Parsec or Megaparsec)
- Build robust parsers for structured data
- Handle complex input formats gracefully

## Problem Description
This week, you'll build a weekend movie watchlist parser and analyzer. The input data is in a more complex format that includes movie details, user ratings, and watch dates in a semi-structured format.

Your task is to create a robust parser that can extract this data and generate useful statistics and recommendations.

## Tasks
1. Use a parsing library to parse the complex input format
2. Build combinators for the different parts of the input
3. Create a data model for the parsed information
4. Generate statistics and recommendations based on the data
5. Format and present the analysis

## Tips
- Install the Parsec or Megaparsec library to help with parsing
- Build your parser incrementally, starting with simple components
- Test each parser combinator separately
- Use the `<|>` operator to handle alternative parsing options
- Don't forget error handling for invalid inputs

## Example
Given input like:
```
MOVIE: The Grand Budapest Hotel (2014)
DIRECTOR: Wes Anderson
GENRES: Comedy, Drama
WATCHED: 2023-06-03
RATING: 4.5/5
NOTES: Visually stunning, great story.

MOVIE: Parasite (2019)
DIRECTOR: Bong Joon-ho
GENRES: Thriller, Drama, Comedy
WATCHED: 2023-06-10
RATING: 5/5
NOTES: Masterpiece, unexpected twists.
```

Your analysis might show:
```
Movie Watchlist Analysis
Total movies watched: 2
Favorite genres: Drama (2), Comedy (2), Thriller (1)
Average rating: 4.75/5
Highest rated: Parasite (5/5)
Recommended next: Other films by Bong Joon-ho
```

## Getting Started
The template.hs file includes imports for Parsec and the basic structure. You'll need to install the parsing library first:

```bash
cabal install parsec
# or
stack install parsec
```

This week will give you practical experience with real-world parsing challenges!