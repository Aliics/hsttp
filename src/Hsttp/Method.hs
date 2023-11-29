module Hsttp.Method (Method (..)) where

data Method
  = Get
  | Put
  | Post
  | Delete
  | Options

instance Show Method where
  show Get = "GET"
  show Put = "PUT"
  show Post = "POST"
  show Delete = "DELETE"
  show Options = "OPTIONS"
