{-# LANGUAGE OverloadedStrings #-}
module HTTP.Render (renderResponse) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Types

-- | Serialize a 'Response' into raw HTTP bytes
renderResponse :: Response -> B.ByteString
renderResponse res = B.concat [statusLine, headerLines, "\r\n", body]
  where
    body = responseBody res
    headers = ensureContentLength body (responseHeaders res)
    statusLine =
         "HTTP/1.1 "
      <> BC.pack (show (statusCode res))
      <> " "
      <> reasonPhrase (statusCode res)
      <> "\r\n"
    headerLines = B.concat [k <> ": " <> v <> "\r\n" | (k,v) <- headers]

ensureContentLength :: B.ByteString -> [Header] -> [Header]
ensureContentLength b hs =
  case lookup "Content-Length" hs of
    Just _  -> hs
    Nothing -> ("Content-Length", BC.pack (show (B.length b))) : hs

reasonPhrase :: Int -> B.ByteString
reasonPhrase code = case code of
  200 -> "OK"
  400 -> "Bad Request"
  404 -> "Not Found"
  500 -> "Internal Server Error"
  _   -> ""
