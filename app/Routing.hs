{-# LANGUAGE OverloadedStrings #-}
module Routing
  ( routeExact
  , methodIs
  , notFound
  ) where

import qualified Data.ByteString as BS
import Middleware
import Types

-- | Default handler returning 404
notFound :: Handler
notFound _ = pure $ notFound404 "Not Found"

-- | Route based on exact path match
routeExact :: BS.ByteString -> Handler -> Middleware
routeExact p h next req =
  if path req == p then h req else next req

-- | Execute handler only when HTTP method matches
methodIs :: BS.ByteString -> Handler -> Middleware
methodIs m h next req =
  if method req == m then h req else next req

