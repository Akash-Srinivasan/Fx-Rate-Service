# FX Rate Service 💱

A production-grade multi-currency exchange rate service built with Haskell. Aggregates rates from multiple sources, provides historical data, and enables real-time monitoring.

## Features

- 🔄 **Multi-Source Aggregation**: Fetches rates from multiple APIs (ExchangeRate-API, ECB)
- 📊 **Weighted Averaging**: Combines rates based on source confidence
- 💾 **Historical Storage**: PostgreSQL database with time-series optimization
- 🚀 **REST API**: Clean, documented endpoints
- ⚡ **Real-time Updates**: Scheduled fetching every 15 minutes
- 🔒 **Type Safety**: Leverages Haskell's type system for correctness
- 🧪 **Tested**: Comprehensive test suite

## Why Haskell?

This project demonstrates why Haskell is excellent for financial systems:

- **Type Safety**: Currency pairs can't be created with same currencies
- **No Runtime Errors**: Catch bugs at compile time
- **Precise Arithmetic**: Using `Scientific` type for accurate decimal handling
- **Concurrent Operations**: Easy async fetching from multiple sources
- **Pure Functions**: Conversion and aggregation logic is testable and reliable

## Quick Start

### Prerequisites

```bash
# Install GHCup (Haskell toolchain manager)
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Install GHC and Cabal
ghcup install ghc 9.6.3
ghcup install cabal 3.10.2.0
ghcup set ghc 9.6.3

# Install PostgreSQL
# macOS
brew install postgresql@15
brew services start postgresql@15

# Ubuntu
sudo apt-get install postgresql-15
```

### Setup Database

```bash
# Create database
createdb fxrates

# Or with psql
psql postgres
CREATE DATABASE fxrates;
\q
```

### Build & Run

```bash
# Clone the project
cd fx-rate-service

# Install dependencies and build
cabal update
cabal build

# Initialize database schema and start API server
cabal run fx-rate-service

# In another terminal, start the rate fetcher
cabal run fx-rate-fetcher
```

The API will be available at `http://localhost:8080`

## Project Structure

```
fx-rate-service/
├── src/
│   └── FXRate/
│       ├── Types.hs              # Core types (Currency, CurrencyPair, etc.)
│       ├── External/
│       │   ├── Types.hs          # API response types
│       │   ├── ExchangeRateAPI.hs # ExchangeRate-API client
│       │   └── ECB.hs            # European Central Bank client
│       ├── Database/
│       │   ├── Schema.hs         # Database schema & migrations
│       │   └── Operations.hs    # CRUD operations
│       ├── Core/
│       │   ├── Aggregation.hs   # Rate aggregation logic
│       │   └── Conversion.hs    # Currency conversion with fees
│       ├── API/
│       │   └── Server.hs         # REST API (Servant)
│       └── Config.hs             # Configuration management
├── app/
│   ├── Main.hs                   # API server executable
│   └── Fetcher.hs                # Rate fetcher cron job
├── test/
│   ├── Spec.hs
│   ├── AggregationSpec.hs        # Aggregation tests
│   └── ConversionSpec.hs         # Conversion tests
└── fx-rate-service.cabal         # Project configuration
```

## API Endpoints

### Health Check
```bash
curl http://localhost:8080/health
```

Response:
```json
{
  "status": "ok",
  "version": "0.1.0"
}
```

### Get Rate for Currency Pair
```bash
curl http://localhost:8080/rates/USD-EUR
```

Response:
```json
{
  "pair": "USD-EUR",
  "rate": 0.9234567,
  "timestamp": "2025-11-03T14:30:00Z",
  "source": "ExchangeRateAPI"
}
```

### Get Live Rates
```bash
curl http://localhost:8080/rates/live/USD
```

Response:
```json
{
  "base": "USD",
  "rates": {
    "EUR": 0.9234567,
    "GBP": 0.7891234,
    "JPY": 149.5678
  }
}
```

## Configuration

Set environment variables:

```bash
# Server
export PORT=8080
export LOG_LEVEL=INFO
export FETCH_INTERVAL=15  # Minutes between rate fetches

# Database
export DB_HOST=localhost
export DB_PORT=5432
export DB_NAME=fxrates
export DB_USER=postgres
export DB_PASSWORD=
```

## Development

### Run Tests
```bash
cabal test
```

### Format Code
```bash
# Install formatter
cabal install ormolu

# Format all files
ormolu --mode inplace $(find . -name '*.hs')
```

### Watch Mode
```bash
# Install ghcid
cabal install ghcid

# Auto-reload on changes
ghcid --command="cabal repl"
```

## Docker Deployment

```bash
# Build image
docker build -t fx-rate-service .

# Run with docker-compose
docker-compose up
```

## Database Schema

```sql
-- Exchange rates table
CREATE TABLE exchange_rates (
  id SERIAL PRIMARY KEY,
  timestamp TIMESTAMPTZ NOT NULL,
  from_currency VARCHAR(3) NOT NULL,
  to_currency VARCHAR(3) NOT NULL,
  rate DECIMAL(20, 10) NOT NULL,
  source VARCHAR(50) NOT NULL,
  confidence VARCHAR(20) NOT NULL,
  bid DECIMAL(20, 10),
  ask DECIMAL(20, 10),
  created_at TIMESTAMPTZ DEFAULT NOW()
);

-- Materialized view for latest rates
CREATE MATERIALIZED VIEW latest_rates AS
  SELECT DISTINCT ON (from_currency, to_currency)
    from_currency,
    to_currency,
    rate,
    timestamp,
    source
  FROM exchange_rates
  ORDER BY from_currency, to_currency, timestamp DESC;
```

## Architecture Highlights

### Type-Safe Currency Pairs
```haskell
-- Prevents creating invalid pairs
mkCurrencyPair :: Currency -> Currency -> Maybe CurrencyPair
mkCurrencyPair from to
  | from == to = Nothing  -- Can't have USD/USD
  | otherwise  = Just $ CurrencyPair from to
```

### Weighted Aggregation
```haskell
-- Aggregate rates based on source confidence
aggregateRates :: [ExchangeRate] -> Maybe AggregatedRate
aggregateRates rates =
  let weightedAvg = sum (weighted rates) / sum (weights rates)
      weighted r = erRate r * confidenceWeight (erConfidence r)
```

### Concurrent Fetching
```haskell
-- Fetch from all sources concurrently
fetchFromAllSources :: Currency -> IO [ExchangeRate]
fetchFromAllSources currency =
  concat <$> mapConcurrently (fetchFromSource currency) allSources
```

## Roadmap

**Week 2-3:**
- [ ] Add more rate sources (Fixer.io, Open Exchange Rates)
- [ ] Implement WebSocket for real-time rates
- [ ] Add rate alerts system
- [ ] Build React frontend dashboard

**Week 4:**
- [ ] Add conversion API with fee calculations
- [ ] Implement rate prediction (ML model)
- [ ] Add API authentication
- [ ] Performance optimization

## Contributing

Contributions welcome! This is a learning project showcasing Haskell for fintech.

## License

MIT License - feel free to use this for learning or as a base for your own projects.

## Resources

- [Haskell in Production: Mercury](https://serokell.io/blog/haskell-in-production-mercury)
- [Servant Tutorial](https://docs.servant.dev/en/stable/tutorial/)
- [Real World Haskell](http://book.realworldhaskell.org/)
