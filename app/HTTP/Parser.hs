{-# LANGUAGE OverloadedStrings #-}

module HTTP.Parser (parseRequest, contentLength) where

import           Control.Applicative       ((<|>))

import Data.Attoparsec.ByteString (Parser, parseOnly, take, many')

import Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Types

parseRequest :: B.ByteString -> Either String Request
parseRequest request = parseOnly httpRequest request

httpRequest :: P.Parser Request
httpRequest = do 
  -- POST /upload HTTP/1.1
  (method, request, httpVer) <- requestLine
  headerList <- parseHeaders
  P.endOfLine

  let contentLengthOutput = contentLength headerList
      transferEncoding = findHeader "Transfer-Encoding" headerList

  case transferEncoding of
    Just enc ->
      if asciiCI enc "chunked"
        then fail "chunked transfer encoding not supported"
        else pure ()
    Nothing -> pure ()

  bodyBytes <- case contentLengthOutput of
    Left err -> fail err
    Right contentLengthHeaders ->
       case contentLengthHeaders of
          Nothing   -> pure B.empty
          Just size -> P.take size 

  let (pathBytes, queryBytes) = splitUri request

  let requestValue = Request {
      method = method,
      path = pathBytes,
      query = queryBytes,
      headers = headerList,
      body = bodyBytes
    }

  pure requestValue

requestLine :: Parser (B.ByteString, B.ByteString, B.ByteString)
requestLine = do
  m   <- token <* sp
  uri <- P.takeTill P.isSpace <* sp
  ver   <- httpVersion
  P.endOfLine
  pure (m, uri, ver)
  where
    sp    = P.char ' '
    token = P.takeTill P.isSpace
    httpVersion =
          (P.string "HTTP/1.1" *> pure "HTTP/1.1")
      <|> (P.string "HTTP/1.0" *> pure "HTTP/1.0")


parseHeaders:: Parser [(B.ByteString, B.ByteString)]
parseHeaders = many' headerLine

headerLine :: Parser (B.ByteString, B.ByteString)
headerLine = do
  -- if next is CRLF, caller will consume it (we do not here)
  name <- P.takeTill (== ':')
  _    <- P.char ':' *> ows
  val  <- P.takeTill (\c -> c == '\r' || c == '\n')
  P.endOfLine
  pure (trim name, trim val)
  where
    ows = P.skipWhile (\c -> c == ' ' || c == '\t')
    trim = BC.takeWhileEnd (/= '\r')


splitUri :: B.ByteString -> (B.ByteString, B.ByteString)
splitUri bs =
  case BC.elemIndex '?' bs of
    Nothing -> (bs, B.empty)
    Just i  -> (B.take i bs, B.drop (i+1) bs)

contentLength :: [(B.ByteString, B.ByteString)] -> Either String (Maybe Int)
contentLength headers =
  case [v | (k,v) <- headers, asciiCI k "Content-Length"] of
    [] -> Right Nothing
    xs -> case traverse (fmap fst . BC.readInt) xs of
            Just (n:rest) | all (== n) rest -> Right (Just n)
            _ -> Left "invalid Content-Length"


-- example headerList [("Host","example.com")...]
findHeader :: B.ByteString -> [(B.ByteString,B.ByteString)] -> Maybe B.ByteString
findHeader headerName headerList =
  case [ value | (name, value) <- headerList, asciiCI name headerName] of
    [] -> Nothing
    (v:_) -> Just v

-- non case sensitive comparison
asciiCI :: BC.ByteString -> BC.ByteString -> Bool
asciiCI a b = BC.map toLowerAscii a == BC.map toLowerAscii b
  where
    toLowerAscii c
      | 'A' <= c && c <= 'Z' = toEnum (fromEnum c + 32)  -- 'A'..'Z' → 'a'..'z'
      | otherwise            = c


