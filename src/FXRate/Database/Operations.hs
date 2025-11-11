{-# LANGUAGE OverloadedStrings #-}

module FXRate.Database.Operations
  ( insertRate
  , insertRates
  , getLatestRate
  , getHistoricalRates
  , connectDB
  , DBConfig(..)
  ) where

import Data.Pool (Pool, createPool, withResource)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple
  ( Connection
  , Query
  , close
  , connect
  , connectDatabase
  , connectHost
  , connectPassword
  , connectPort
  , connectUser
  , defaultConnectInfo
  , execute
  , query
  )
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import FXRate.Types

-- | Database configuration
data DBConfig = DBConfig
  { dbHost     :: String
  , dbPort     :: Int
  , dbName     :: String
  , dbUser     :: String
  , dbPassword :: String
  }
  deriving (Show)

-- | Default local PostgreSQL config
defaultDBConfig :: DBConfig
defaultDBConfig = DBConfig
  { dbHost = "localhost"
  , dbPort = 5432
  , dbName = "fxrates"
  , dbUser = "postgres"
  , dbPassword = ""
  }

-- | Connect to database
connectDB :: DBConfig -> IO Connection
connectDB config = connect defaultConnectInfo
  { connectHost = dbHost config
  , connectPort = fromIntegral $ dbPort config
  , connectDatabase = dbName config
  , connectUser = dbUser config
  , connectPassword = dbPassword config
  }

-- | Create connection pool
createDBPool :: DBConfig -> IO (Pool Connection)
createDBPool config =
  createPool
    (connectDB config)
    close
    1  -- number of stripes
    60 -- idle time (seconds)
    10 -- max connections

-- | Insert a single exchange rate
insertRate :: Connection -> ExchangeRate -> IO ()
insertRate conn rate = do
  let CurrencyPair (Currency from) (Currency to) = erPair rate
  execute conn insertSQL
    ( erTimestamp rate
    , from
    , to
    , erRate rate
    , sourceToText (erSource rate)
    , confidenceToText (erConfidence rate)
    , erBid rate
    , erAsk rate
    )
  return ()
  where
    insertSQL =
      "INSERT INTO exchange_rates \
      \  (timestamp, from_currency, to_currency, rate, source, confidence, bid, ask) \
      \VALUES (?, ?, ?, ?, ?, ?, ?, ?)"

-- | Insert multiple exchange rates efficiently
insertRates :: Connection -> [ExchangeRate] -> IO ()
insertRates conn rates = mapM_ (insertRate conn) rates

-- | Database row result
data RateRow = RateRow
  { rrFromCurrency :: Text
  , rrToCurrency   :: Text
  , rrRate         :: Scientific
  , rrTimestamp    :: UTCTime
  , rrSource       :: Text
  , rrConfidence   :: Text
  , rrBid          :: Maybe Scientific
  , rrAsk          :: Maybe Scientific
  }

instance FromRow RateRow where
  fromRow = RateRow
    <$> field  -- from_currency
    <*> field  -- to_currency
    <*> field  -- rate
    <*> field  -- timestamp
    <*> field  -- source
    <*> field  -- confidence
    <*> field  -- bid
    <*> field  -- ask

-- | Convert RateRow to ExchangeRate
rowToExchangeRate :: RateRow -> ExchangeRate
rowToExchangeRate row = ExchangeRate
  { erPair = CurrencyPair (Currency $ rrFromCurrency row) (Currency $ rrToCurrency row)
  , erRate = rrRate row
  , erTimestamp = rrTimestamp row
  , erSource = textToSource (rrSource row)
  , erConfidence = textToConfidence (rrConfidence row)
  , erBid = rrBid row
  , erAsk = rrAsk row
  }

-- | Get the latest rate for a currency pair
getLatestRate :: Connection -> CurrencyPair -> IO (Maybe ExchangeRate)
getLatestRate conn (CurrencyPair (Currency from) (Currency to)) = do
  rows <- query conn selectSQL (from, to) :: IO [RateRow]
  return $ case rows of
    (row:_) -> Just $ rowToExchangeRate row
    [] -> Nothing
  where
    selectSQL =
      "SELECT from_currency, to_currency, rate, timestamp, source, confidence, bid, ask \
      \FROM exchange_rates \
      \WHERE from_currency = ? AND to_currency = ? \
      \ORDER BY timestamp DESC \
      \LIMIT 1"

-- | Get historical rates for a currency pair within a time range
getHistoricalRates :: Connection -> CurrencyPair -> UTCTime -> UTCTime -> IO [ExchangeRate]
getHistoricalRates conn (CurrencyPair (Currency from) (Currency to)) startTime endTime = do
  rows <- query conn selectSQL (from, to, startTime, endTime) :: IO [RateRow]
  return $ map rowToExchangeRate rows
  where
    selectSQL =
      "SELECT from_currency, to_currency, rate, timestamp, source, confidence, bid, ask \
      \FROM exchange_rates \
      \WHERE from_currency = ? AND to_currency = ? \
      \  AND timestamp >= ? AND timestamp <= ? \
      \ORDER BY timestamp DESC"

-- | Helper functions for converting enums to/from text
sourceToText :: Source -> Text
sourceToText ExchangeRateAPI = "ExchangeRateAPI"
sourceToText ECB = "ECB"
sourceToText Fixer = "Fixer"
sourceToText OpenExchangeRates = "OpenExchangeRates"

textToSource :: Text -> Source
textToSource "ExchangeRateAPI" = ExchangeRateAPI
textToSource "ECB" = ECB
textToSource "Fixer" = Fixer
textToSource "OpenExchangeRates" = OpenExchangeRates
textToSource _ = ExchangeRateAPI  -- Default fallback

confidenceToText :: Confidence -> Text
confidenceToText High = "High"
confidenceToText Medium = "Medium"
confidenceToText Low = "Low"

textToConfidence :: Text -> Confidence
textToConfidence "High" = High
textToConfidence "Medium" = Medium
textToConfidence "Low" = Low
textToConfidence _ = Medium  -- Default fallback
