.PHONY: all clean deps help test

# Default target
all: help

# Check dependencies
deps:
	@echo "Checking dependencies..."
	@./scripts/deps.sh

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

# Show help information
help:
	@echo "Lazy Weekends - Learn Haskell at Your Own Pace"
	@echo ""
	@echo "Available targets:"
	@echo "  deps    - Check for required dependencies"
	@echo "  clean   - Remove build artifacts"
	@echo "  test    - Run tests for each week (if available)"
	@echo "  help    - Display this help message"
	@echo ""
	@echo "Getting started:"
	@echo "  1. Run 'make deps' to check your environment"
	@echo "  2. Navigate to week1/ to begin"
	@echo "  3. Read the README.md in each week's directory"
	@echo ""
	@echo "See the main README.md for more information."