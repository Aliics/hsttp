module Hsttp.Header (mkHeaderString) where

mkHeaderString :: [(String, String)] -> [String]
mkHeaderString = fmap (\(k, v) -> k ++ ": " ++ v ++ "\r\n")
