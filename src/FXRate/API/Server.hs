{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module FXRate.API.Server
  ( app
  , API
  , server
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON, encode, object, (.=))
import qualified Data.Aeson.Key as Key
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection)
import FXRate.Core.Aggregation (fetchFromAllSources, getAggregatedForPair)
import FXRate.Database.Operations (getLatestRate)
import FXRate.Types
import Network.Wai (Application)
import Servant
  ( Capture
  , Get
  , Handler
  , JSON
  , Proxy(..)
  , Server
  , err404
  , err500
  , errBody
  , serve
  , throwError
  , (:<|>)(..)
  , (:>)
  )
import Data.Scientific
import Data.Time
import Data.Aeson.Types (ToJSON(toJSON))

-- | API definition
type API = 
       "health" :> Get '[JSON] HealthResponse
  :<|> "rates" :> Capture "pair" Text :> Get '[JSON] RateResponse
  :<|> "rates" :> "live" :> Capture "base" Text :> Get '[JSON] LiveRatesResponse

-- | Health check response
data HealthResponse = HealthResponse
  { status :: Text
  , version :: Text
  }
  deriving (Show)

instance ToJSON HealthResponse where
  toJSON (HealthResponse s v) = object
    [ "status" .= s
    , "version" .= v
    ]

-- | Rate response
data RateResponse = RateResponse
  { pair :: Text
  , rate :: Scientific
  , timestamp :: UTCTime
  , source :: Text
  }
  deriving (Show)

instance ToJSON RateResponse where
  toJSON (RateResponse p r ts src) = object
    [ "pair" .= p
    , "rate" .= r
    , "timestamp" .= ts
    , "source" .= src
    ]

-- | Live rates response
data LiveRatesResponse = LiveRatesResponse
  { base :: Text
  , rates :: [(Text, Scientific)]
  }
  deriving (Show)

instance ToJSON LiveRatesResponse where
  toJSON (LiveRatesResponse b rs) = object
    [ "base" .= b
    , "rates" .= object [ Key.fromText k .= v | (k, v) <- rs ]
    ]

-- | Server implementation
server :: Connection -> Server API
server conn = 
       healthHandler
  :<|> rateHandler conn
  :<|> liveRatesHandler

-- | Health check handler
healthHandler :: Handler HealthResponse
healthHandler = return $ HealthResponse "ok" "0.1.0"

-- | Get rate for a specific pair
rateHandler :: Connection -> Text -> Handler RateResponse
rateHandler conn pairText = do
  case parsePair pairText of
    Nothing -> throwError err404 { errBody = "Invalid currency pair format. Use USD-EUR" }
    Just pair -> do
      maybeRate <- liftIO $ getLatestRate conn pair
      case maybeRate of
        Nothing -> throwError err404 { errBody = "No rate found for this pair" }
        Just rate -> return $ RateResponse
          { pair = pairText
          , rate = erRate rate
          , timestamp = erTimestamp rate
          , source = T.pack $ show $ erSource rate
          }

-- | Get live rates from all sources
liveRatesHandler :: Text -> Handler LiveRatesResponse
liveRatesHandler baseText = do
  let baseCurrency = Currency baseText
  rates <- liftIO $ fetchFromAllSources baseCurrency
  let rateList = [ (unCurrency $ toCurrency $ erPair r, erRate r) | r <- rates ]
  return $ LiveRatesResponse baseText rateList

-- | Parse currency pair from text (e.g., "USD-EUR")
parsePair :: Text -> Maybe CurrencyPair
parsePair text =
  case T.splitOn "-" text of
    [from, to] -> mkCurrencyPair (Currency from) (Currency to)
    _ -> Nothing

-- | Create WAI application
app :: Connection -> Application
app conn = serve (Proxy :: Proxy API) (server conn)