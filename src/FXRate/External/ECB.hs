{-# LANGUAGE OverloadedStrings #-}

module FXRate.External.ECB
  ( fetchECBRates
  ) where

import Control.Exception (SomeException, catch)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import FXRate.Types
import Network.HTTP.Simple (getResponseBody, httpBS, parseRequest)
import System.Timeout (timeout)
import Text.XML (def, parseLBS)
import Text.XML (Node(..), Element(..), elementAttributes, Name(..))
import Text.XML.Cursor
  ( Cursor
  , element
  , fromDocument
  , node
  , ($//), (>=>)
  )
import qualified Data.ByteString.Lazy as BL

-- | ECB Daily rates URL (free, no API key needed)
ecbDailyURL :: String
ecbDailyURL = "https://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml"

-- | Fetch exchange rates from ECB (base is always EUR)
fetchECBRates :: IO (Either String [ExchangeRate])
fetchECBRates = do
  result <- timeout (5 * 1000000) fetchECBRatesInternal
  case result of
    Nothing -> return $ Left "ECB request timed out"
    Just rates -> return rates

-- | Internal fetch implementation
fetchECBRatesInternal :: IO (Either String [ExchangeRate])
fetchECBRatesInternal = do
  result <- (Right <$> downloadXML) `catch` handleException
  case result of
    Left err -> return $ Left err
    Right xmlData -> parseXMLRates xmlData
  where
    handleException :: SomeException -> IO (Either String a)
    handleException e = return $ Left $ "ECB HTTP error: " ++ show e

-- | Download XML from ECB
downloadXML :: IO BL.ByteString
downloadXML = do
  request <- parseRequest ecbDailyURL
  response <- httpBS request
  return $ BL.fromStrict $ getResponseBody response

-- | Parse ECB XML format
parseXMLRates :: BL.ByteString -> IO (Either String [ExchangeRate])
parseXMLRates xmlData = do
  timestamp <- getCurrentTime  -- ECB updates daily, use current time
  case parseLBS def xmlData of
    Left err -> return $ Left $ "XML parse error: " ++ show err
    Right doc -> do
      let cursor = fromDocument doc
          rates = extractRates cursor
      return $ Right $ map (toExchangeRate timestamp) rates

-- | Extract currency rates from XML cursor
extractRates :: Cursor -> [(Text, Scientific)]
extractRates cursor =
  cursor $// element "{http://www.ecb.int/vocabulary/2002-08-01/eurofxref}Cube"
         >=> \c -> do
               let attrs = node c
               case attrs of
                 NodeElement el -> 
                   let attrMap = elementAttributes el
                       currencyName = Name "currency" (Just "http://www.ecb.int/vocabulary/2002-08-01/eurofxref") Nothing
                       rateName = Name "rate" (Just "http://www.ecb.int/vocabulary/2002-08-01/eurofxref") Nothing
                   in case (Map.lookup currencyName attrMap, Map.lookup rateName attrMap) of
                     (Just currency, Just rate) ->
                       case reads (T.unpack rate) of
                         [(rateVal, "")] -> return (currency, rateVal)
                         _ -> []
                     _ -> []
                 _ -> []

-- | Convert ECB rate to our ExchangeRate type
toExchangeRate :: UTCTime -> (Text, Scientific) -> ExchangeRate
toExchangeRate timestamp (currencyCode, rate) =
  ExchangeRate
    { erPair = CurrencyPair (Currency "EUR") (Currency currencyCode)
    , erRate = rate
    , erTimestamp = timestamp
    , erSource = ECB
    , erConfidence = High  -- ECB is highly trusted
    , erBid = Nothing
    , erAsk = Nothing
    }
