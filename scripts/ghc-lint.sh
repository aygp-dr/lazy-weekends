#!/usr/bin/env bash
# GHC linting script for Haskell files

set -e

check_file() {
    local file="$1"
    echo "Checking $file with GHC..."
    ghc -Wall -fno-code -fno-warn-name-shadowing "$file"
}

# Find all Haskell files
find_haskell_files() {
    find . -name "*.hs" | grep -v "/dist/" | grep -v "/.stack-work/" | sort
}

# Clean up any temporary files
cleanup() {
    find . -name "*.o" -type f -delete
    find . -name "*.hi" -type f -delete
    find . -name "*.dyn_o" -type f -delete
    find . -name "*.dyn_hi" -type f -delete
}

# Run GHC check on all files
main() {
    local has_errors=0
    
    for file in $(find_haskell_files); do
        if ! check_file "$file"; then
            has_errors=1
        fi
    done
    
    cleanup
    
    if [[ $has_errors -eq 1 ]]; then
        echo "GHC found issues in the code"
        exit 1
    else
        echo "All files passed GHC checks"
    fi
}

main "$@"