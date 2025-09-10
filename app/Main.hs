{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           BuiltinMiddleware
import           Routing
import           Server
import           Middleware
import           Types

main :: IO ()
main = run "127.0.0.1:8080" $
       notFound
       |-> logMw
       |-> corsMw
       |-> routeExact "/ping" (const $ pure $ ok200 "pong")
