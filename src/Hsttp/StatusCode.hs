module Hsttp.StatusCode (StatusCode (..)) where

data StatusCode
  = OK
  | BadRequest
  | InternalServerError

instance Show StatusCode where
  show OK = "200 OK"
  show BadRequest = "400 Bad Request"
  show InternalServerError = "500 Internal Server Error"

instance Read StatusCode where
  readsPrec _ s@"200 OK" = [(OK, s)]
  readsPrec _ s@"400 Bad Request" = [(BadRequest, s)]
  readsPrec _ s@"500 Internal Server Error" = [(InternalServerError, s)]
  readsPrec _ s = error $ "Unknown status code " ++ s 
