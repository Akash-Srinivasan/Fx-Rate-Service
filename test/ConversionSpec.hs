{-# LANGUAGE OverloadedStrings #-}

module ConversionSpec (spec) where

import FXRate.Core.Conversion
import FXRate.Types
import Test.Hspec

spec :: Spec
spec = do
  describe "convert" $ do
    it "converts amounts correctly" $ do
      convert 100 0.92 `shouldBe` 92
      convert 1000 1.5 `shouldBe` 1500
    
    it "handles zero rate" $ do
      convert 100 0 `shouldBe` 0
  
  describe "convertWithFees" $ do
    it "calculates fees correctly" $ do
      let pair = CurrencyPair (Currency "USD") (Currency "EUR")
          midRate = 0.92
          fees = FeeStructure
            { feeSpread = 0.01      -- 1% markup
            , feeFixed = 1          -- 1 EUR fixed fee
            , feePercentage = 0.01  -- 1% of amount
            }
          result = convertWithFees 100 pair midRate fees
      
      -- Applied rate should be lower than mid-market
      convAppliedRate result `shouldSatisfy` (< midRate)
      
      -- Should have fees
      convTotalFees result `shouldSatisfy` (> 0)
      
      -- Destination amount should be less than mid-market conversion
      convDestAmount result `shouldSatisfy` (< 92)
