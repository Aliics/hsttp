module Hsttp.Constants
  ( crlf,
    chunkSize,
    httpVersion,
  )
where

httpVersion :: String
httpVersion = "HTTP/1.1"

crlf :: String
crlf = "\r\n"

chunkSize :: Int
chunkSize = 1024
