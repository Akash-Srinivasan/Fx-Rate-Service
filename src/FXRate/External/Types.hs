{-# LANGUAGE DeriveAnyClass #-}

module FXRate.External.Types where

import Data.Aeson (FromJSON(..), ToJSON, withObject, (.:))
import Data.Map.Strict (Map)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | ExchangeRate-API response format
data ExchangeRateAPIResponse = ExchangeRateAPIResponse
  { erApiBase      :: !Text
  , erApiRates     :: !(Map Text Scientific)
  , erApiDate      :: !Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

-- Custom FromJSON to map JSON fields to our prefixed field names
instance FromJSON ExchangeRateAPIResponse where
  parseJSON = withObject "ExchangeRateAPIResponse" $ \v -> ExchangeRateAPIResponse
    <$> v .: "base"
    <*> v .: "rates"
    <*> v .: "date"

-- | ECB Daily Rates XML structure (simplified)
data ECBDailyRates = ECBDailyRates
  { ecbDate  :: !Text
  , ecbRates :: ![(Text, Scientific)]
  }
  deriving stock (Generic, Show)

-- | Fixer.io response format
data FixerResponse = FixerResponse
  { success   :: !Bool
  , timestamp :: !Int
  , base      :: !Text
  , date      :: !Text
  , rates     :: !(Map Text Scientific)
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)