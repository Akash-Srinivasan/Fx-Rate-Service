{-# LANGUAGE OverloadedStrings #-}

module Main where

import FXRate.API.Server (app)
import FXRate.Config (Config(..), loadConfig)
import FXRate.Database.Operations (connectDB)
import FXRate.Database.Schema (initSchema)
import Network.Wai.Handler.Warp (run)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  -- Load configuration
  config <- loadConfig
  putStrLn "🚀 Starting FX Rate Service..."
  putStrLn $ "Configuration loaded: Port " ++ show (cfgPort config)
  
  -- Connect to database
  putStrLn "Connecting to database..."
  conn <- connectDB (cfgDBConfig config)
  putStrLn "✓ Database connected"
  
  -- Initialize schema
  initSchema conn
  
  -- Start server
  putStrLn $ "🌐 Server running on http://localhost:" ++ show (cfgPort config)
  putStrLn "Endpoints:"
  putStrLn "  GET /health"
  putStrLn "  GET /rates/:pair (e.g., /rates/USD-EUR)"
  putStrLn "  GET /rates/live/:base (e.g., /rates/live/USD)"
  putStrLn ""
  putStrLn "Press Ctrl+C to stop"
  
  run (cfgPort config) (app conn)
