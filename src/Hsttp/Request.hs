module Hsttp.Request (Request (..)) where

import Hsttp.Method

data Request = Request
  { requestMethod :: Method,
    requestUri :: String,
    requestHeaders :: [(String, String)],
    requestBody :: IO String
  }
