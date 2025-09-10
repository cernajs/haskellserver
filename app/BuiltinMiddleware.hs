{-# LANGUAGE OverloadedStrings #-}
module BuiltinMiddleware
  ( logMw
  , corsMw
  , recoverMw
  ) where

import qualified Data.ByteString.Char8 as BC
import           Control.Exception (SomeException, try)
import           Middleware
import           Types

-- | Simple logging middleware
logMw :: Middleware
logMw next req = do
  putStrLn $ BC.unpack (method req) ++ " " ++ BC.unpack (path req)
  res <- next req
  putStrLn $ "-> " ++ show (statusCode res)
  pure res

-- | Add permissive CORS headers
corsMw :: Middleware
corsMw next req = do
  res <- next req
  let hs = ("Access-Control-Allow-Origin", "*") : responseHeaders res
  pure res { responseHeaders = hs }

-- | Recover from exceptions and return 500 response
recoverMw :: Middleware
recoverMw next req = do
  result <- (try (next req) :: IO (Either SomeException Response))
  case result of
    Right res -> pure res
    Left _    -> pure $ internalServerError500 "Internal Server Error"
