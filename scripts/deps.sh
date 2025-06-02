#!/bin/bash
# Check Haskell dependencies for Lazy Weekends project

echo "Checking Haskell environment..."

# Check if GHC is installed
if command -v ghc >/dev/null 2>&1; then
    GHC_VERSION=$(ghc --version | awk '{print $NF}')
    echo "✓ GHC installed (version $GHC_VERSION)"
else
    echo "✗ GHC not found. Please install GHC:"
    echo "  - macOS: brew install ghc"
    echo "  - Ubuntu: apt install ghc"
    echo "  - FreeBSD: pkg install ghc"
    echo "  - Or use GHCup: https://www.haskell.org/ghcup/"
    exit 1
fi

# Check if Cabal is installed
if command -v cabal >/dev/null 2>&1; then
    CABAL_VERSION=$(cabal --version | head -n 1 | awk '{print $NF}')
    echo "✓ Cabal installed (version $CABAL_VERSION)"
else
    echo "✗ Cabal not found. Please install Cabal:"
    echo "  - macOS: brew install cabal-install"
    echo "  - Ubuntu: apt install cabal-install"
    echo "  - FreeBSD: pkg install hs-cabal-install"
    echo "  - Or use GHCup: https://www.haskell.org/ghcup/"
    exit 1
fi

# Check for optional tools

# Check if Stack is installed
if command -v stack >/dev/null 2>&1; then
    STACK_VERSION=$(stack --version | awk '{print $NF}')
    echo "✓ Stack installed (version $STACK_VERSION)"
else
    echo "i Stack not found. This is optional but recommended for larger projects."
    echo "  - Install from: https://docs.haskellstack.org/en/stable/install_and_upgrade/"
fi

# Check if HLint is installed
if command -v hlint >/dev/null 2>&1; then
    HLINT_VERSION=$(hlint --version | head -n 1 | awk '{print $NF}')
    echo "✓ HLint installed (version $HLINT_VERSION)"
else
    echo "i HLint not found. This is optional but recommended for code quality."
    echo "  - Install with: cabal install hlint"
fi

# Check for common libraries
echo ""
echo "Checking for common libraries..."

# Function to check if a package is installed
check_package() {
    PACKAGE=$1
    if ghc-pkg list | grep -q "$PACKAGE"; then
        echo "✓ $PACKAGE installed"
        return 0
    else
        echo "i $PACKAGE not installed. This may be needed for some weeks."
        return 1
    fi
}

# Check for some common packages
check_package "parsec"
check_package "time"
check_package "directory"
check_package "containers"
check_package "optparse-applicative"

echo ""
echo "Environment check complete."
echo "If you're missing any packages, you can install them with:"
echo "  cabal install <package-name>"
echo ""
echo "Happy Haskell learning!"