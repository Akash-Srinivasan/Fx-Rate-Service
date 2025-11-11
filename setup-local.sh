#!/bin/bash

# Local Development Setup Script (No Docker Required)
# This script helps you run the FX Rate Service locally

set -e

echo "🚀 FX Rate Service - Local Setup"
echo "================================="
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Check if GHC is installed
if ! command -v ghc &> /dev/null; then
    echo -e "${RED}✗ GHC (Haskell compiler) not found${NC}"
    echo ""
    echo "Please install GHCup first:"
    echo "curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh"
    echo ""
    exit 1
fi

echo -e "${GREEN}✓ GHC found:${NC} $(ghc --version)"

# Check if Cabal is installed
if ! command -v cabal &> /dev/null; then
    echo -e "${RED}✗ Cabal not found${NC}"
    echo "Install with: ghcup install cabal"
    exit 1
fi

echo -e "${GREEN}✓ Cabal found:${NC} $(cabal --version | head -1)"

# Check if PostgreSQL is installed
if ! command -v psql &> /dev/null; then
    echo -e "${YELLOW}⚠ PostgreSQL not found${NC}"
    echo ""
    echo "Please install PostgreSQL:"
    echo "  macOS: brew install postgresql@15"
    echo "  Linux: sudo apt-get install postgresql-15"
    echo ""
    exit 1
fi

echo -e "${GREEN}✓ PostgreSQL found${NC}"

# Check if database exists
if psql -lqt | cut -d \| -f 1 | grep -qw fxrates; then
    echo -e "${GREEN}✓ Database 'fxrates' exists${NC}"
else
    echo -e "${YELLOW}⚠ Database 'fxrates' not found${NC}"
    echo "Creating database..."
    createdb fxrates
    echo -e "${GREEN}✓ Database created${NC}"
fi

# Update Cabal package list
echo ""
echo "📦 Updating Cabal package list..."
cabal update

# Install dependencies
echo ""
echo "📦 Installing dependencies (this may take 10-15 minutes first time)..."
cabal build --only-dependencies

# Build the project
echo ""
echo "🔨 Building the project..."
cabal build

echo ""
echo -e "${GREEN}✅ Build complete!${NC}"
echo ""
echo "🎉 You can now run:"
echo ""
echo "  Terminal 1 - API Server:"
echo "    cabal run fx-rate-service"
echo ""
echo "  Terminal 2 - Rate Fetcher:"
echo "    cabal run fx-rate-fetcher"
echo ""
echo "  Test it:"
echo "    curl http://localhost:8080/health"
echo ""
echo "Happy coding! 🚀"
