{-# LANGUAGE DeriveAnyClass #-}

module FXRate.Types
  ( Currency(..)
  , CurrencyPair(..)
  , ExchangeRate(..)
  , Source(..)
  , Confidence(..)
  , AggregatedRate(..)
  , mkCurrencyPair
  , invertPair
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | ISO 4217 currency code
newtype Currency = Currency { unCurrency :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

-- | Currency pair (e.g., USD/EUR)
data CurrencyPair = CurrencyPair
  { fromCurrency :: !Currency
  , toCurrency   :: !Currency
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Smart constructor for currency pair (prevents same currency pairs)
mkCurrencyPair :: Currency -> Currency -> Maybe CurrencyPair
mkCurrencyPair from to
  | from == to = Nothing
  | otherwise  = Just $ CurrencyPair from to

-- | Invert a currency pair (USD/EUR -> EUR/USD)
invertPair :: CurrencyPair -> CurrencyPair
invertPair (CurrencyPair from to) = CurrencyPair to from

-- | Exchange rate source
data Source
  = ExchangeRateAPI
  | ECB  -- European Central Bank
  | Fixer
  | OpenExchangeRates
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Confidence level in a rate
data Confidence
  = High
  | Medium
  | Low
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Exchange rate from a single source
data ExchangeRate = ExchangeRate
  { erPair       :: !CurrencyPair
  , erRate       :: !Scientific
  , erTimestamp  :: !UTCTime
  , erSource     :: !Source
  , erConfidence :: !Confidence
  , erBid        :: !(Maybe Scientific)
  , erAsk        :: !(Maybe Scientific)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Aggregated rate from multiple sources
data AggregatedRate = AggregatedRate
  { aggrRate           :: !Scientific
  , aggrSpread         :: !Scientific  -- Difference between highest and lowest
  , aggrSources        :: ![ExchangeRate]
  , aggrTimestamp      :: !UTCTime
  , aggrStalenessMs    :: !Int  -- How old is the oldest rate
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
