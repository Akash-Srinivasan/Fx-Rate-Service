{-# LANGUAGE OverloadedStrings #-}

module AggregationSpec (spec) where

import Data.Time (getCurrentTime)
import FXRate.Core.Aggregation (aggregateRates, confidenceWeight)
import FXRate.Types
import Test.Hspec

spec :: Spec
spec = do
  describe "aggregateRates" $ do
    it "returns Nothing for empty list" $ do
      aggregateRates [] `shouldBe` Nothing
    
    it "calculates correct weighted average" $ do
      time <- getCurrentTime
      let rate1 = ExchangeRate
            { erPair = CurrencyPair (Currency "USD") (Currency "EUR")
            , erRate = 0.92
            , erTimestamp = time
            , erSource = ExchangeRateAPI
            , erConfidence = High
            , erBid = Nothing
            , erAsk = Nothing
            }
          rate2 = rate1 { erRate = 0.94, erConfidence = Low }
          result = aggregateRates [rate1, rate2]
      
      case result of
        Nothing -> expectationFailure "Expected aggregated rate"
        Just aggr -> do
          -- High confidence (0.92 * 1.0) + Low confidence (0.94 * 0.3) = 1.202
          -- Total weight: 1.0 + 0.3 = 1.3
          -- Average: 1.202 / 1.3 = 0.924615...
          aggrRate aggr `shouldSatisfy` (\r -> r > 0.924 && r < 0.925)
  
  describe "confidenceWeight" $ do
    it "returns correct weights" $ do
      confidenceWeight High `shouldBe` 1.0
      confidenceWeight Medium `shouldBe` 0.7
      confidenceWeight Low `shouldBe` 0.3
