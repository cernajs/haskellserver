module Middleware where

import Types

type Handler = Request -> IO Respones

type Middleware = Handler -> Handler

(|->) :: Handler -> Middleware -> Handler
handler |-> middleware = middleware handler
