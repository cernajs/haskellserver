module Types where

import qualified Data.ByteString as BS

type Header = (BS.ByteString, BS.ByteString)

data Request = Request {
  method::BS.ByteString,
  path::BS.ByteString,
  query :: BS.ByteString,
  headers::[Header],
  body::BS.ByteString
} deriving (Show, Eq)


data Response = Response {
  statusCode::Int,
  responseHeaders::[Header],
  responseBody::BS.ByteString
} deriving (Show, Eq)

-- | Construct a response with given status and body
response :: Int -> BS.ByteString -> Response
response s b = Response s [] b

-- | Status helpers
ok200, notFound404, badRequest400, internalServerError500 :: BS.ByteString -> Response
ok200                = response 200
notFound404          = response 404
badRequest400        = response 400
internalServerError500 = response 500

-- | Add a header to a response
addHeader :: Header -> Response -> Response
addHeader h r = r { responseHeaders = h : responseHeaders r }
