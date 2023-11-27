module Hsttp.Response (Response (..), marshalResponse) where

import Hsttp.StatusCode
import Hsttp.Header

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
