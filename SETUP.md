# Setup Guide - Week 1

This guide will walk you through setting up the FX Rate Service from scratch.

## Prerequisites Installation

### macOS

```bash
# Install Homebrew if you don't have it
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Install GHCup (Haskell toolchain)
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Follow the prompts, then reload your shell
source ~/.ghcup/env

# Install GHC and Cabal
ghcup install ghc 9.6.3
ghcup install cabal 3.10.2.0
ghcup set ghc 9.6.3

# Verify installation
ghc --version
cabal --version

# Install PostgreSQL
brew install postgresql@15
brew services start postgresql@15
```

### Linux (Ubuntu/Debian)

```bash
# Install dependencies
sudo apt-get update
sudo apt-get install -y curl build-essential libgmp-dev libffi-dev libncurses-dev libtinfo-dev

# Install GHCup
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Follow prompts and reload shell
source ~/.ghcup/env

# Install GHC and Cabal
ghcup install ghc 9.6.3
ghcup install cabal 3.10.2.0
ghcup set ghc 9.6.3

# Install PostgreSQL
sudo apt-get install -y postgresql-15 postgresql-contrib-15
sudo systemctl start postgresql
sudo systemctl enable postgresql
```

### Windows

```powershell
# Install GHCup
Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true } catch { Write-Error $_ }

# Install PostgreSQL
# Download and install from: https://www.postgresql.org/download/windows/
```

## Project Setup

### 1. Navigate to the project directory

```bash
cd fx-rate-service
```

### 2. Install project dependencies

```bash
# Update Cabal package index
cabal update

# Build and install dependencies (this may take 10-15 minutes first time)
cabal build --only-dependencies
```

### 3. Set up the database

```bash
# Create the database
createdb fxrates

# Or if you need to specify user:
createdb -U postgres fxrates

# If you get permission errors:
sudo -u postgres createdb fxrates
```

### 4. Configure environment (optional)

```bash
# Copy example env file
cp .env.example .env

# Edit .env with your settings if needed
nano .env
```

### 5. Build the project

```bash
cabal build
```

This will compile:
- `fx-rate-service` - The API server
- `fx-rate-fetcher` - The rate fetching service
- Tests

## Running the Application

### Option 1: Using Cabal (Development)

**Terminal 1 - API Server:**
```bash
cabal run fx-rate-service
# Server will start on http://localhost:8080
```

**Terminal 2 - Rate Fetcher:**
```bash
cabal run fx-rate-fetcher
# Will fetch rates every 15 minutes
```

### Option 2: Using Docker (Production-like)

```bash
# Build and start all services
docker-compose up --build

# Or run in background
docker-compose up -d

# View logs
docker-compose logs -f

# Stop services
docker-compose down
```

### Option 3: Using Makefile

```bash
# Build
make build

# Run API server
make run

# In another terminal, run fetcher
make fetch

# Or use Docker
make docker-up
```

## Testing the API

Once the server is running, test it:

```bash
# Health check
curl http://localhost:8080/health

# Get a specific rate (after fetcher has run once)
curl http://localhost:8080/rates/USD-EUR

# Get live rates from external APIs
curl http://localhost:8080/rates/live/USD
```

## Common Issues & Solutions

### Issue: "command not found: cabal"

**Solution:** Make sure GHCup environment is loaded:
```bash
source ~/.ghcup/env
# Or add to your ~/.bashrc or ~/.zshrc
```

### Issue: "database 'fxrates' does not exist"

**Solution:** Create the database:
```bash
createdb fxrates
```

### Issue: "connection to server failed: Connection refused"

**Solution:** Start PostgreSQL:
```bash
# macOS
brew services start postgresql@15

# Linux
sudo systemctl start postgresql
```

### Issue: Cabal build fails with dependency errors

**Solution:** Clean and rebuild:
```bash
cabal clean
cabal update
cabal build
```

### Issue: "Could not resolve dependencies"

**Solution:** Try with specific solver:
```bash
cabal build --allow-newer
# Or specify constraints
cabal build --constraint="base>=4.16"
```

## Development Workflow

### 1. Auto-reload during development

```bash
# Install ghcid
cabal install ghcid

# Run with auto-reload
ghcid --command="cabal repl" --test=":main"
```

### 2. Running tests

```bash
cabal test

# Or with detailed output
cabal test --test-show-details=direct
```

### 3. Code formatting

```bash
# Install ormolu
cabal install ormolu

# Format all files
make format
```

## What You've Built

After Week 1, you now have:

✅ A working Haskell project with proper structure  
✅ Multi-source rate fetching (ExchangeRate-API, ECB)  
✅ PostgreSQL database with schema and operations  
✅ REST API with three endpoints  
✅ Concurrent rate aggregation  
✅ Type-safe domain models  
✅ Automated rate fetching  
✅ Test suite  
✅ Docker deployment  

## Next Steps (Week 2)

- Add more API endpoints (conversion, historical data)
- Implement rate alerts
- Build React frontend
- Add WebSocket support for real-time updates
- Add more external sources
- Implement caching with Redis

## Resources

- [Haskell Documentation](https://www.haskell.org/documentation/)
- [Servant Tutorial](https://docs.servant.dev/en/stable/tutorial/)
- [PostgreSQL-Simple Guide](https://hackage.haskell.org/package/postgresql-simple)
- [Aeson Tutorial](https://artyom.me/aeson)

## Getting Help

If you run into issues:

1. Check the [README.md](README.md)
2. Review error messages carefully
3. Check PostgreSQL is running: `pg_isready`
4. Verify Haskell installation: `ghc --version && cabal --version`
5. Look at example code in `src/` directory
6. Search [Haskell Discourse](https://discourse.haskell.org/)

Good luck! 🚀
