module Hsttp.Response
  ( Response (..),
    marshalResponse,
    readResponseHead,
  )
where

import Data.Bifunctor (bimap)
import Data.Extra.List
import Hsttp.Constants (chunkSize, crlf, httpVersion)
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
  pure $ httpVersion ++ " " ++ sc ++ crlf ++ hs ++ crlf ++ body

readResponseHead :: Socket -> IO (String, String)
readResponseHead s = do
  maybeChunk <- recv s chunkSize
  let r@(h, b) = readHeadAndSplit (show <$> maybeChunk)
  if b /= ""
    then pure r
    else fmap (bimap (h ++) (b ++)) (readResponseHead s)

readHeadAndSplit :: Maybe String -> (String, String)
readHeadAndSplit (Just chunk) =
  case indexOf chunk crlf of
    (Just n) -> splitAt n chunk
    Nothing -> (show chunk, "")
readHeadAndSplit Nothing = ("", "")
