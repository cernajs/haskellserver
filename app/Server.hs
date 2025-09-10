{-# LANGUAGE OverloadedStrings #-}
module Server (run) where

import           Control.Monad (forever)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import           Network.Socket
import           Network.Socket.ByteString (recv, sendAll)
import           Middleware
import           Types
import qualified HTTP.Parser as P
import qualified HTTP.Render as R

-- | Run the server on the given host:port, handling each connection sequentially
run :: String -> Handler -> IO ()
run addr handler = withSocketsDo $ do
  let (host, portStr) = break (== ':') addr
      port = drop 1 portStr
  putStrLn $ "[DEBUG] Starting server on " ++ host ++ ":" ++ port

  addrInfos <- getAddrInfo (Just defaultHints{addrFlags=[AI_PASSIVE]}) (Just host) (Just port)
  let serverAddr = head addrInfos
  -- setup TCP socket
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress serverAddr)
  listen sock 10
  putStrLn "[DEBUG] Server listening..."

  forever $ do
    (conn, peer) <- accept sock
    putStrLn $ "[DEBUG] Accepted connection from " ++ show peer
    handleConn conn
  where
    handleConn conn = do
      msg <- recvRequest conn 
      putStrLn $ "[DEBUG] Raw request received:\n" ++ C8.unpack msg
      case P.parseRequest msg of
        Left err -> sendAll conn $ R.renderResponse $ badRequest400 "Bad Request"
        Right req -> do
          res <- handler req
          sendAll conn $ R.renderResponse res
      close conn



recvN :: Socket -> Int -> IO BS.ByteString
recvN s n = go BS.empty n
  where
    go acc 0 = pure acc
    go acc k = do
      chunk <- recv s (min 4096 k)
      if BS.null chunk then pure acc else go (acc <> chunk) (k - BS.length chunk)

recvRequest :: Socket -> IO BS.ByteString
recvRequest s = do
  hdrsPlus <- readUntilEndOfHeaders BS.empty
  let (hdrs, rest) = BS.breakSubstring "\r\n\r\n" hdrsPlus
      headersBlock = hdrs
      afterHdrs    = BS.drop 4 rest  -- drop "\r\n\r\n"

      -- parse headers into [(k,v)]
      headerLines =
        [ (k, C8.dropWhile (== ' ') (C8.drop 1 v))
        | l <- C8.lines (crlfToLf headersBlock)
        , let line = C8.takeWhile (/= '\r') l
        , not (BS.null line)
        , let (k,v) = C8.break (== ':') line
        , not (BS.null v)
        ]

      mLenE = P.contentLength headerLines

  case mLenE of
    Left err -> pure (hdrs <> "\r\n\r\n")  -- invalid Content-Length: just return headers
    Right (Just n) ->
      if BS.length afterHdrs >= n
        then pure (hdrs <> "\r\n\r\n" <> afterHdrs)
        else do body <- recvN s (n - BS.length afterHdrs)
                pure (hdrs <> "\r\n\r\n" <> afterHdrs <> body)
    Right Nothing ->
      pure (hdrs <> "\r\n\r\n") -- no body

  where
    readUntilEndOfHeaders acc = do
      chunk <- recv s 4096
      if BS.null chunk
        then pure acc
        else let acc' = acc <> chunk
              in if "\r\n\r\n" `BS.isInfixOf` acc'
                   then pure acc'
                   else readUntilEndOfHeaders acc'

    crlfToLf = C8.map (\c -> if c == '\r' then '\n' else c)
