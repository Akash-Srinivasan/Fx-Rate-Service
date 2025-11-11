{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Time (getCurrentTime)
import FXRate.Config (Config(..), loadConfig)
import FXRate.Core.Aggregation (fetchFromAllSources)
import FXRate.Database.Operations (connectDB, insertRates)
import FXRate.Database.Schema (refreshLatestRates)
import FXRate.Types (Currency(..), ExchangeRate)


-- | List of major currencies to fetch
majorCurrencies :: [Currency]
majorCurrencies =
  [ Currency "USD"
  , Currency "EUR"
  , Currency "GBP"
  , Currency "JPY"
  , Currency "CHF"
  , Currency "CAD"
  , Currency "AUD"
  ]

main :: IO ()
main = do
  config <- loadConfig
  putStrLn "🔄 FX Rate Fetcher starting..."
  putStrLn $ "Fetch interval: " ++ show (cfgFetchInterval config) ++ " minutes"
  
  -- Connect to database
  conn <- connectDB (cfgDBConfig config)
  putStrLn "✓ Database connected"
  
  -- Run fetch loop
  forever $ do
    timestamp <- getCurrentTime
    putStrLn $ "\n📊 Fetching rates at " ++ show timestamp
    
    -- Fetch rates for each major currency
    allRates <- concat <$> mapM fetchAndLog majorCurrencies
    
    -- Insert into database
    putStrLn $ "💾 Inserting " ++ show (length allRates) ++ " rates into database..."
    insertRates conn allRates
    putStrLn "✓ Rates saved"
    
    -- Refresh materialized view
    putStrLn "🔄 Refreshing latest rates view..."
    refreshLatestRates conn
    
    putStrLn "✅ Fetch cycle complete"
    putStrLn $ "⏰ Next fetch in " ++ show (cfgFetchInterval config) ++ " minutes"
    
    -- Wait for next cycle
    threadDelay (cfgFetchInterval config * 60 * 1000000)  -- Convert minutes to microseconds

-- | Fetch rates for a currency and log results
fetchAndLog :: Currency -> IO [ExchangeRate]
fetchAndLog currency = do
  putStrLn $ "  Fetching " ++ show currency ++ "..."
  rates <- fetchFromAllSources currency
  putStrLn $ "  ✓ Got " ++ show (length rates) ++ " rates"
  return rates

-- Import missing type
