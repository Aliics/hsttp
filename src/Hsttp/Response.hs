module Hsttp.Response (Response (..), marshalResponse) where

import Hsttp.StatusCode
  
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

mkHeaderString :: [(String, String)] -> [String]
mkHeaderString = fmap (\(k, v) -> k ++ ": " ++ v ++ "\r\n")
