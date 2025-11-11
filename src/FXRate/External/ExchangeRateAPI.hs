{-# LANGUAGE OverloadedStrings #-}

module FXRate.External.ExchangeRateAPI
  ( fetchRates
  , fetchRatesWithRetry
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import FXRate.External.Types (ExchangeRateAPIResponse(..))
import FXRate.Types
import Network.HTTP.Simple
  ( Request
  , getResponseBody
  , httpBS
  , parseRequest
  , setRequestQueryString
  )
import System.Timeout (timeout)

-- | Base URL for ExchangeRate-API
baseURL :: String
baseURL = "https://api.exchangerate-api.com/v4/latest/"

-- | Fetch exchange rates for a base currency
-- Free tier doesn't require API key, but has rate limits
fetchRates :: Currency -> IO (Either String [ExchangeRate])
fetchRates (Currency base) = do
  let url = baseURL ++ T.unpack base
  result <- timeout (30 * 1000000) $ fetchRatesInternal url  -- 30 second timeout
  case result of
    Nothing -> return $ Left "Request timed out after 30 seconds"
    Just rates -> return rates

-- | Internal fetch with error handling
fetchRatesInternal :: String -> IO (Either String [ExchangeRate])
fetchRatesInternal url = do
  result <- (Right <$> makeRequest url) `catch` handleException
  case result of
    Left err -> return $ Left err
    Right response -> parseResponse response
  where
    handleException :: SomeException -> IO (Either String a)
    handleException e = return $ Left $ "HTTP error: " ++ show e

-- | Make HTTP request
makeRequest :: String -> IO ExchangeRateAPIResponse
makeRequest url = do
  request <- parseRequest url
  response <- httpBS request
  case eitherDecode (BL.fromStrict $ getResponseBody response) of
    Left err -> error $ "JSON decode error: " ++ err
    Right val -> return val

-- | Parse API response into ExchangeRate list
parseResponse :: ExchangeRateAPIResponse -> IO (Either String [ExchangeRate])
parseResponse apiResponse = do
  let baseCurrency = Currency $ erApiBase apiResponse
      ratesMap = erApiRates apiResponse
  timestamp <- getCurrentTime
  
  return $ Right $ mapToExchangeRates baseCurrency ratesMap timestamp

-- | Convert Map of rates to ExchangeRate list
mapToExchangeRates :: Currency -> Map Text Scientific -> UTCTime -> [ExchangeRate]
mapToExchangeRates base ratesMap timestamp =
  [ ExchangeRate
      { erPair = CurrencyPair base (Currency targetCurrency)
      , erRate = rate
      , erTimestamp = timestamp
      , erSource = ExchangeRateAPI
      , erConfidence = High
      , erBid = Nothing
      , erAsk = Nothing
      }
  | (targetCurrency, rate) <- Map.toList ratesMap
  , Currency targetCurrency /= base  -- Don't include base/base pair
  ]

-- | Fetch rates with exponential backoff retry
fetchRatesWithRetry :: Currency -> Int -> IO (Either String [ExchangeRate])
fetchRatesWithRetry currency maxRetries = retry maxRetries 0
  where
    retry 0 _ = return $ Left "Max retries exceeded"
    retry retriesLeft attemptNum = do
      result <- fetchRates currency
      case result of
        Right rates -> return $ Right rates
        Left err -> do
          putStrLn $ "  Attempt " ++ show (maxRetries - retriesLeft + 1) ++ " failed: " ++ err
          let delayMs = (2 ^ attemptNum) * 1000000  -- Exponential backoff in microseconds
          threadDelay delayMs
          retry (retriesLeft - 1) (attemptNum + 1)