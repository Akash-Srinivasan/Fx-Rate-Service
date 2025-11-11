{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import qualified AggregationSpec
import qualified ConversionSpec

main :: IO ()
main = hspec $ do
  describe "Rate Aggregation" AggregationSpec.spec
  describe "Currency Conversion" ConversionSpec.spec
