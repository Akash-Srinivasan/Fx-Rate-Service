#!/bin/bash

# FX Rate Service - Local Run Script
# Use this if Docker is giving you network issues

set -e

echo "🚀 FX Rate Service - Local Setup"
echo "=================================="
echo ""

# Check if PostgreSQL is running
if ! command -v pg_isready &> /dev/null; then
    echo "❌ PostgreSQL is not installed"
    echo ""
    echo "Install PostgreSQL:"
    echo "  macOS:   brew install postgresql@15 && brew services start postgresql@15"
    echo "  Ubuntu:  sudo apt-get install postgresql-15"
    echo "  Arch:    sudo pacman -S postgresql"
    echo ""
    exit 1
fi

# Check if PostgreSQL is running
if ! pg_isready -q; then
    echo "❌ PostgreSQL is not running"
    echo ""
    echo "Start PostgreSQL:"
    echo "  macOS:   brew services start postgresql@15"
    echo "  Linux:   sudo systemctl start postgresql"
    echo ""
    exit 1
fi

echo "✓ PostgreSQL is running"

# Check if database exists
if ! psql -lqt | cut -d \| -f 1 | grep -qw fxrates; then
    echo "Creating database 'fxrates'..."
    createdb fxrates
    echo "✓ Database created"
else
    echo "✓ Database 'fxrates' exists"
fi

# Check if Haskell is installed
if ! command -v cabal &> /dev/null; then
    echo "❌ Cabal is not installed"
    echo ""
    echo "Install Haskell toolchain:"
    echo "  curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh"
    echo "  source ~/.ghcup/env"
    echo "  ghcup install ghc 9.6.3"
    echo "  ghcup install cabal 3.10.2.0"
    echo ""
    exit 1
fi

echo "✓ Cabal is installed"
echo ""

# Set environment variables
export DB_HOST=localhost
export DB_PORT=5432
export DB_NAME=fxrates
export DB_USER=${USER}
export DB_PASSWORD=
export PORT=8080
export LOG_LEVEL=INFO

echo "Environment configured:"
echo "  DB_HOST: $DB_HOST"
echo "  DB_PORT: $DB_PORT"
echo "  DB_NAME: $DB_NAME"
echo "  PORT: $PORT"
echo ""

# Build the project
echo "Building project..."
cabal update
cabal build
echo "✓ Build complete"
echo ""

# Offer to run the services
echo "Choose what to run:"
echo "  1) API Server only"
echo "  2) Rate Fetcher only"
echo "  3) Both (in background)"
echo ""
read -p "Choice (1-3): " choice

case $choice in
    1)
        echo ""
        echo "🌐 Starting API server on http://localhost:8080"
        echo "Press Ctrl+C to stop"
        echo ""
        cabal run fx-rate-service
        ;;
    2)
        echo ""
        echo "🔄 Starting rate fetcher"
        echo "Press Ctrl+C to stop"
        echo ""
        cabal run fx-rate-fetcher
        ;;
    3)
        echo ""
        echo "Starting both services..."
        
        # Start API in background
        cabal run fx-rate-service &
        API_PID=$!
        
        # Wait a bit for API to start
        sleep 2
        
        # Start fetcher in background
        cabal run fx-rate-fetcher &
        FETCHER_PID=$!
        
        echo ""
        echo "✓ Services started!"
        echo "  API Server: http://localhost:8080 (PID: $API_PID)"
        echo "  Fetcher: Running (PID: $FETCHER_PID)"
        echo ""
        echo "Test the API:"
        echo "  curl http://localhost:8080/health"
        echo "  curl http://localhost:8080/rates/live/USD"
        echo ""
        echo "To stop:"
        echo "  kill $API_PID $FETCHER_PID"
        echo ""
        echo "Logs will appear here. Press Ctrl+C to stop both services."
        
        # Wait for user interrupt
        trap "kill $API_PID $FETCHER_PID 2>/dev/null; echo ''; echo 'Services stopped'; exit" INT
        wait
        ;;
    *)
        echo "Invalid choice"
        exit 1
        ;;
esac
