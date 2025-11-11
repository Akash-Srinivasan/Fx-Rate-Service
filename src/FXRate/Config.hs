{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module FXRate.Config
  ( Config(..)
  , loadConfig
  , defaultConfig
  ) where

import FXRate.Database.Operations (DBConfig(..))
import GHC.Generics (Generic)
import System.Environment (lookupEnv)

-- | Application configuration
data Config = Config
  { cfgPort       :: Int
  , cfgDBConfig   :: DBConfig
  , cfgLogLevel   :: String
  , cfgFetchInterval :: Int  -- Minutes between rate fetches
  }
  deriving (Show, Generic)

-- | Default configuration
defaultConfig :: Config
defaultConfig = Config
  { cfgPort = 8080
  , cfgDBConfig = DBConfig
      { dbHost = "localhost"
      , dbPort = 5432
      , dbName = "fxrates"
      , dbUser = "postgres"
      , dbPassword = ""
      }
  , cfgLogLevel = "INFO"
  , cfgFetchInterval = 15  -- Fetch every 15 minutes
  }

-- | Load configuration from environment variables
loadConfig :: IO Config
loadConfig = do
  port <- lookupEnvInt "PORT" (cfgPort defaultConfig)
  dbHost <- lookupEnvStr "DB_HOST" (dbHost $ cfgDBConfig defaultConfig)
  dbPort <- lookupEnvInt "DB_PORT" (dbPort $ cfgDBConfig defaultConfig)
  dbName <- lookupEnvStr "DB_NAME" (dbName $ cfgDBConfig defaultConfig)
  dbUser <- lookupEnvStr "DB_USER" (dbUser $ cfgDBConfig defaultConfig)
  dbPassword <- lookupEnvStr "DB_PASSWORD" (dbPassword $ cfgDBConfig defaultConfig)
  logLevel <- lookupEnvStr "LOG_LEVEL" (cfgLogLevel defaultConfig)
  fetchInterval <- lookupEnvInt "FETCH_INTERVAL" (cfgFetchInterval defaultConfig)
  
  return Config
    { cfgPort = port
    , cfgDBConfig = DBConfig
        { dbHost = dbHost
        , dbPort = dbPort
        , dbName = dbName
        , dbUser = dbUser
        , dbPassword = dbPassword
        }
    , cfgLogLevel = logLevel
    , cfgFetchInterval = fetchInterval
    }

-- | Helper to lookup environment variable as Int
lookupEnvInt :: String -> Int -> IO Int
lookupEnvInt key defaultVal = do
  maybeVal <- lookupEnv key
  case maybeVal of
    Nothing -> return defaultVal
    Just val -> case reads val of
      [(num, "")] -> return num
      _ -> return defaultVal

-- | Helper to lookup environment variable as String
lookupEnvStr :: String -> String -> IO String
lookupEnvStr key defaultVal = do
  maybeVal <- lookupEnv key
  return $ maybe defaultVal id maybeVal
