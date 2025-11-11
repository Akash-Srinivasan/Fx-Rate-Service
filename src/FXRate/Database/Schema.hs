{-# LANGUAGE OverloadedStrings #-}

module FXRate.Database.Schema where

import qualified Data.Text as T
import qualified Data.Text.Encoding
import Database.PostgreSQL.Simple
  ( Connection
  , Only(..)
  , Query
  , execute_
  , query_
  )

-- | SQL to create the exchange_rates table
createRatesTableSQL :: Query
createRatesTableSQL = 
  "CREATE TABLE IF NOT EXISTS exchange_rates ( \
  \  id SERIAL PRIMARY KEY, \
  \  timestamp TIMESTAMPTZ NOT NULL, \
  \  from_currency VARCHAR(3) NOT NULL, \
  \  to_currency VARCHAR(3) NOT NULL, \
  \  rate DECIMAL(20, 10) NOT NULL, \
  \  source VARCHAR(50) NOT NULL, \
  \  confidence VARCHAR(20) NOT NULL, \
  \  bid DECIMAL(20, 10), \
  \  ask DECIMAL(20, 10), \
  \  created_at TIMESTAMPTZ DEFAULT NOW() \
  \)"

-- | Create index on timestamp for faster queries
createTimestampIndexSQL :: Query
createTimestampIndexSQL =
  "CREATE INDEX IF NOT EXISTS idx_rates_timestamp \
  \  ON exchange_rates(timestamp DESC)"

-- | Create index on currency pair
createPairIndexSQL :: Query
createPairIndexSQL =
  "CREATE INDEX IF NOT EXISTS idx_rates_pair \
  \  ON exchange_rates(from_currency, to_currency, timestamp DESC)"

-- | Create materialized view for latest rates
createLatestRatesViewSQL :: Query
createLatestRatesViewSQL =
  "CREATE MATERIALIZED VIEW IF NOT EXISTS latest_rates AS \
  \  SELECT DISTINCT ON (from_currency, to_currency) \
  \    from_currency, \
  \    to_currency, \
  \    rate, \
  \    timestamp, \
  \    source \
  \  FROM exchange_rates \
  \  ORDER BY from_currency, to_currency, timestamp DESC"

-- | Create index on materialized view
createLatestRatesIndexSQL :: Query
createLatestRatesIndexSQL =
  "CREATE UNIQUE INDEX IF NOT EXISTS idx_latest_rates_pair \
  \  ON latest_rates(from_currency, to_currency)"

-- | Initialize database schema
initSchema :: Connection -> IO ()
initSchema conn = do
  putStrLn "Creating database schema..."
  
  -- Create tables
  execute_ conn createRatesTableSQL
  putStrLn "✓ Created exchange_rates table"
  
  -- Create indexes
  execute_ conn createTimestampIndexSQL
  execute_ conn createPairIndexSQL
  putStrLn "✓ Created indexes"
  
  -- Create materialized view
  execute_ conn createLatestRatesViewSQL
  execute_ conn createLatestRatesIndexSQL
  putStrLn "✓ Created latest_rates materialized view"
  
  putStrLn "Database schema initialized successfully!"

-- | Refresh the latest_rates materialized view
refreshLatestRates :: Connection -> IO ()
refreshLatestRates conn = do
  execute_ conn "REFRESH MATERIALIZED VIEW latest_rates"
  putStrLn "✓ Refreshed latest_rates view"