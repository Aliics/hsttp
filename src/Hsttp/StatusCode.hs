module Hsttp.StatusCode (StatusCode (..)) where

data StatusCode
  = OK
  | BadRequest
  | InternalServerError

instance Show StatusCode where
  show OK = "200 OK"
  show BadRequest = "400 Bad Request"
  show InternalServerError = "500 Internal Server Error"
