module Middleware where

import Types

type Handler = Request -> IO Response

type Middleware = Handler -> Handler

(|->) :: Handler -> Middleware -> Handler
handler |-> middleware = middleware handler
