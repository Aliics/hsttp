{-# OverloadedStrings #-}

module Hsttp.Response (Response (..), marshalResponse) where

import Data.Extra.List
import Hsttp.Header
import Hsttp.StatusCode
import Network.Simple.TCP

data Response = Response
  { responseStatusCode :: StatusCode,
    responseHeaders :: [(String, String)],
    responseBody :: IO String
  }

marshalResponse :: Response -> IO String
marshalResponse r = do
  body <- responseBody r
  let sc = show (responseStatusCode r)
  let hs = unwords $ mkHeaderString $ responseHeaders r
  pure $ "HTTP/1.1 " ++ sc ++ "\r\n" ++ hs ++ "\r\n" ++ body

--recvResponse :: Socket -> IO Response
--recvResponse s = do
--  (head, bodyOverflow) <- recv s 1024
--  pure Response {}
--
readResponseHead :: Socket -> IO (String, String)
readResponseHead s = do
  maybeBs <- recv s 1024
  case maybeBs of
    (Just bs) ->
      if show bs `contains` "\r\n"
        then pure ("", "")
        else pure ("", "")
    Nothing -> pure ("", "")
