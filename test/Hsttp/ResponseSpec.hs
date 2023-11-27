{-# LANGUAGE RankNTypes #-}

module Hsttp.ResponseSpec (spec) where

import Hsttp.Response
import Hsttp.StatusCode
import Test.Hspec
import Hsttp.Tests

spec :: Spec
spec =
  describeTable
    "marshalResponse"
    marshalResponse
    [ TableTest
        { name = "should create simple 200 ok response",
          given =
            Response
              { responseStatusCode = OK,
                responseHeaders = [],
                responseBody = pure ""
              },
          want = "HTTP/1.1 200 OK\r\n\r\n"
        },
      TableTest
        { name = "should create 400 bad request with headers",
          given =
            Response
              { responseStatusCode = BadRequest,
                responseHeaders = [("Content-Type", "plain/text")],
                responseBody = pure ""
              },
          want = "HTTP/1.1 400 Bad Request\r\nContent-Type: plain/text\r\n\r\n"
        }
    ]
