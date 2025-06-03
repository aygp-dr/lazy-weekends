.PHONY: all clean deps help test spoilers hlint

# Default target
all: help

# Check dependencies
deps:
	@echo "Checking dependencies..."
	@/usr/bin/env bash ./scripts/deps.sh

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	@find . -name "*.o" -type f -delete
	@find . -name "*.hi" -type f -delete
	@find . -name "*.dyn_o" -type f -delete
	@find . -name "*.dyn_hi" -type f -delete
	@rm -rf dist dist-newstyle .stack-work

# Run tests for each week
test:
	@echo "Running tests..."
	@for week in week*; do \
		if [ -f $$week/test.hs ]; then \
			echo "Testing $$week..."; \
			(cd $$week && ghc -o test test.hs && ./test); \
		fi \
	done

# Run the solution for a specific week
spoilers:
	@if [ -z "$(WEEK)" ]; then \
		echo "Usage: make spoilers WEEK=week1"; \
		echo "Or use: make spoilers WEEK=all to run all weeks"; \
	else \
		if [ "$(WEEK)" = "all" ]; then \
			for week in week*; do \
				echo "\n\n==== $$week Solution ===="; \
				if [ -f $$week/spoilers/solution.hs ]; then \
					(cd $$week && ghc -o solution spoilers/solution.hs && ./solution); \
				else \
					echo "No solution available for $$week"; \
				fi; \
			done; \
		else \
			echo "\n==== $(WEEK) Solution ===="; \
			if [ -f $(WEEK)/spoilers/solution.hs ]; then \
				(cd $(WEEK) && ghc -o solution spoilers/solution.hs && ./solution); \
			else \
				echo "No solution available for $(WEEK)"; \
			fi; \
		fi; \
	fi

# Install and run HLint
hlint:
	@echo "Checking if HLint is installed..."
	@if command -v hlint >/dev/null 2>&1; then \
		echo "HLint found: $$(hlint --version)"; \
	else \
		echo "Installing HLint..."; \
		cabal install hlint; \
	fi
	@echo "Running HLint on code..."
	@for week in week*; do \
		if [ -d $$week ]; then \
			echo "Checking $$week..."; \
			hlint $$week --ignore="Use camelCase" || true; \
		fi \
	done

# Show help information
help:
	@echo "Lazy Weekends - Learn Haskell at Your Own Pace"
	@echo ""
	@echo "Available targets:"
	@echo "  deps     - Check for required dependencies"
	@echo "  clean    - Remove build artifacts"
	@echo "  test     - Run tests for each week (if available)"
	@echo "  spoilers - Run the solution for a specific week (make spoilers WEEK=week1)"
	@echo "  hlint    - Run HLint to check code quality"
	@echo "  help     - Display this help message"
	@echo ""
	@echo "Getting started:"
	@echo "  1. Run 'make deps' to check your environment"
	@echo "  2. Navigate to week1/ to begin"
	@echo "  3. Read the README.md in each week's directory"
	@echo ""
	@echo "See the main README.md for more information."