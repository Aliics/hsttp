module Hsttp.RequestSpec (spec) where

import qualified Hsttp.Method as Method
import Hsttp.Request
import Test.Hspec
import Test.Table

spec :: Spec
spec =
  describeTable
    "marshalRequest"
    marshalRequest
    [ TableTest
        { name = "should create simple get to /",
          given =
            Request
              { requestMethod = Method.Get,
                requestUri = "/",
                requestHeaders = [],
                requestBody = pure ""
              },
          want = "GET / HTTP/1.1\r\n\r\n"
        },
      TableTest
        { name = "should create post with headers and body",
          given =
            Request
              { requestMethod = Method.Post,
                requestUri = "/rabbits",
                requestHeaders = [("Content-Type", "application/json")],
                requestBody = pure "{}"
              },
          want = "POST /rabbits HTTP/1.1\r\nContent-Type: application/json\r\n\r\n{}"
        }
    ]
