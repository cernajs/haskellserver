{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           BuiltinMiddleware
import           Routing
import           Server
import           Middleware
import           Types

-- main :: IO ()
-- main = run "127.0.0.1:8080" $
--        notFound
--        |-> logMw
--        |-> corsMw
--        |-> routeExact "/ping" (const $ pure $ ok200 "pong")

-- main :: IO ()
-- main = run "127.0.0.1:8080" $
--        notFound
--        |-> logMw
--        |-> routeExact "/echo" (
--               guardMethod "POST" $ \req ->
--                 pure $ ok200 (body req)
--           )

-- main :: IO ()
-- main = run "127.0.0.1:8080" $
--        notFound
--        |-> logMw
--        |-> corsMw
--        |-> routeExact "/api/time" (const $ do
--              pure $ jsonResponse "{\"time\":\"2025-09-10T12:00:00Z\"}"
--           )

main :: IO ()
main = run "127.0.0.1:8080" $
       notFound
       |-> logMw
       |-> corsMw
       |-> routeExact "/ping" (const $ pure $ ok200 "pong")
       |-> routeExact "/hello" (const $ pure $ ok200 "Hi there!")
       |-> routeExact "/bye" (const $ pure $ ok200 "Goodbye 👋")
