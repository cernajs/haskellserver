module Types where

type Header = (String, String)

data Request = Request {
  method::String,
  path::String,
  headers::[Header],
  body::String
} deriving Show


data Respones = Response {
  statusCode::Int,
  responseHeaders::[Header],
  responseBody::String
} deriving Show
