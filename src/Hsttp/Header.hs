module Hsttp.Header (mkHeaderString) where

import Hsttp.Constants (crlf)

mkHeaderString :: [(String, String)] -> [String]
mkHeaderString = fmap (\(k, v) -> k ++ ": " ++ v ++ crlf)
