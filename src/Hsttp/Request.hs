module Hsttp.Request (Request (..), marshalRequest) where

import Hsttp.Method
import Hsttp.Header
import Hsttp.Constants (crlf, httpVersion)

data Request = Request
  { requestMethod :: Method,
    requestUri :: String,
    requestHeaders :: [(String, String)],
    requestBody :: IO String
  }
  
marshalRequest :: Request -> IO String
marshalRequest r = do
  body <- requestBody r
  let m = show $ requestMethod r
  let u = requestUri r
  let hs = unwords $ mkHeaderString $ requestHeaders r 
  pure $ m ++ " " ++ u ++ " " ++ httpVersion ++ crlf ++ hs ++ crlf ++ body
