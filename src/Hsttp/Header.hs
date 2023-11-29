module Hsttp.Header (mkHeaderString, readHeaderLines, headerValue) where

import Data.Extra.List (indexOf)
import Hsttp.Constants (crlf)

mkHeaderString :: [(String, String)] -> [String]
mkHeaderString = fmap (\(k, v) -> k ++ ": " ++ v ++ crlf)

readHeaderLines :: [String] -> [(String, String)]
readHeaderLines [] = []
readHeaderLines (l : ls) =
  case indexOf l ": " of
    (Just i) -> splitAt i l : readHeaderLines ls
    Nothing -> readHeaderLines ls

headerValue :: String -> [(String, String)] -> Maybe String
headerValue _ [] = Nothing
headerValue s ((k, v) : hs)
  | s == k = Just v
  | otherwise = headerValue s hs
