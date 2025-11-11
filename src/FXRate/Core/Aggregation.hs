{-# LANGUAGE OverloadedStrings #-}

module FXRate.Core.Aggregation
  ( aggregateRates
  , fetchFromAllSources
  , confidenceWeight
  , getAggregatedForPair
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)
import Data.Scientific (Scientific)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import FXRate.External.ECB (fetchECBRates)
import FXRate.External.ExchangeRateAPI (fetchRatesWithRetry)
import FXRate.Types
import qualified Data.Text as T

-- | Fetch rates from all available sources concurrently
fetchFromAllSources :: Currency -> IO [ExchangeRate]
fetchFromAllSources baseCurrency = do
  -- Fetch from multiple sources concurrently
  results <- mapConcurrently (fetchFromSource baseCurrency) allSources
  -- Flatten and filter out errors
  return $ concat [ rates | Right rates <- results ]

-- | All available rate sources
allSources :: [Source]
allSources = [ExchangeRateAPI, ECB]

-- | Fetch from a specific source
fetchFromSource :: Currency -> Source -> IO (Either String [ExchangeRate])
fetchFromSource currency ExchangeRateAPI = do
  result <- fetchRatesWithRetry currency 3
  case result of
    Left err -> do
      putStrLn $ "❌ ExchangeRateAPI error: " ++ err
      return $ Left err
    Right rates -> return $ Right rates
fetchFromSource _ ECB = do
  result <- fetchECBRates
  case result of
    Left err -> do
      putStrLn $ "❌ ECB error: " ++ err
      return $ Left err
    Right rates -> return $ Right rates
fetchFromSource _ _ = return $ Right []  -- Other sources not implemented yet

-- | Aggregate multiple exchange rates for the same currency pair
aggregateRates :: [ExchangeRate] -> Maybe AggregatedRate
aggregateRates [] = Nothing
aggregateRates rates = Just $ AggregatedRate
  { aggrRate = weightedAverage rates
  , aggrSpread = calculateSpread rates
  , aggrSources = rates
  , aggrTimestamp = maximum $ map erTimestamp rates
  , aggrStalenessMs = calculateStaleness rates
  }

-- | Calculate weighted average of rates based on confidence
weightedAverage :: [ExchangeRate] -> Scientific
weightedAverage rates =
  sum weightedRates / sum weights
  where
    weightedRates = [ erRate r * confidenceWeight (erConfidence r) | r <- rates ]
    weights = [ confidenceWeight (erConfidence r) | r <- rates ]

-- | Get numeric weight for confidence level
confidenceWeight :: Confidence -> Scientific
confidenceWeight High = 1.0
confidenceWeight Medium = 0.7
confidenceWeight Low = 0.3

-- | Calculate spread (difference between highest and lowest rate)
calculateSpread :: [ExchangeRate] -> Scientific
calculateSpread [] = 0
calculateSpread rates =
  erRate maxRate - erRate minRate
  where
    maxRate = maximumBy (comparing erRate) rates
    minRate = minimumBy (comparing erRate) rates

-- | Calculate staleness in milliseconds (age of oldest rate)
calculateStaleness :: [ExchangeRate] -> Int
calculateStaleness [] = 0
calculateStaleness rates =
  let oldestTime = minimum $ map erTimestamp rates
      newestTime = maximum $ map erTimestamp rates
      diff = diffUTCTime newestTime oldestTime
  in round (diff * 1000)  -- Convert to milliseconds

-- | Filter rates for a specific currency pair
filterPair :: CurrencyPair -> [ExchangeRate] -> [ExchangeRate]
filterPair pair = filter (\r -> erPair r == pair)

-- | Get aggregated rate for a specific pair from a list of rates
getAggregatedForPair :: CurrencyPair -> [ExchangeRate] -> Maybe AggregatedRate
getAggregatedForPair pair rates =
  aggregateRates $ filterPair pair rates