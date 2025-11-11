{-# LANGUAGE OverloadedStrings #-}

module FXRate.Core.Conversion
  ( convert
  , convertWithFees
  , FeeStructure(..)
  , Conversion(..)
  , FeeBreakdown(..)
  ) where

import Data.Scientific (Scientific)
import FXRate.Types

-- | Fee structure for conversions
data FeeStructure = FeeStructure
  { feeSpread     :: Scientific  -- Markup on rate (e.g., 0.02 = 2%)
  , feeFixed      :: Scientific  -- Fixed fee in destination currency
  , feePercentage :: Scientific  -- Percentage of amount
  }

-- | Default fee structure (mid-market, no fees)
noFees :: FeeStructure
noFees = FeeStructure
  { feeSpread = 0
  , feeFixed = 0
  , feePercentage = 0
  }

-- | Typical fintech fee structure (competitive)
fintechFees :: FeeStructure
fintechFees = FeeStructure
  { feeSpread = 0.005      -- 0.5% markup
  , feeFixed = 0           -- No fixed fee
  , feePercentage = 0.01   -- 1% of amount
  }

-- | Traditional bank fee structure (higher)
bankFees :: FeeStructure
bankFees = FeeStructure
  { feeSpread = 0.03       -- 3% markup
  , feeFixed = 5           -- $5 fixed fee
  , feePercentage = 0.02   -- 2% of amount
  }

-- | Fee breakdown
data FeeBreakdown = FeeBreakdown
  { fbPercentageFee :: Scientific
  , fbFixedFee      :: Scientific
  , fbSpreadCost    :: Scientific
  }

-- | Conversion result
data Conversion = Conversion
  { convSourceAmount      :: Scientific
  , convSourceCurrency    :: Currency
  , convDestAmount        :: Scientific
  , convDestCurrency      :: Currency
  , convAppliedRate       :: Scientific
  , convMidMarketRate     :: Scientific
  , convTotalFees         :: Scientific
  , convFeeBreakdown      :: FeeBreakdown
  }

-- | Simple conversion without fees
convert :: Scientific -> Scientific -> Scientific
convert amount rate = amount * rate

-- | Conversion with fee structure
convertWithFees :: Scientific -> CurrencyPair -> Scientific -> FeeStructure -> Conversion
convertWithFees amount pair midRate fees =
  let markedUpRate = midRate * (1 - feeSpread fees)
      grossAmount = amount * markedUpRate
      percentFee = grossAmount * feePercentage fees
      spreadCost = amount * midRate * feeSpread fees
      totalFees = percentFee + feeFixed fees + spreadCost
      netAmount = amount * midRate - totalFees
  in Conversion
       { convSourceAmount = amount
       , convSourceCurrency = fromCurrency pair
       , convDestAmount = netAmount
       , convDestCurrency = toCurrency pair
       , convAppliedRate = markedUpRate
       , convMidMarketRate = midRate
       , convTotalFees = totalFees
       , convFeeBreakdown = FeeBreakdown percentFee (feeFixed fees) spreadCost
       }

-- | Calculate effective rate (what user actually gets)
effectiveRate :: Conversion -> Scientific
effectiveRate conv = convDestAmount conv / convSourceAmount conv

-- | Calculate how much better mid-market is vs applied rate
savingsVsMidMarket :: Conversion -> Scientific
savingsVsMidMarket conv =
  (convSourceAmount conv * convMidMarketRate conv) - convDestAmount conv
