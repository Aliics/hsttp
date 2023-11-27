module Data.Extra.ListSpec (spec) where

import Data.Extra.List
import Test.Hspec

spec :: Spec
spec = do
  describe "contains" $ do
    it "should return True because b is in a" $ do
      ("abc" `contains` "b") `shouldBe` True
      ("Ollie is awesome" `contains` "Ollie") `shouldBe` True
      ("Spongehead" `contains` "head") `shouldBe` True
    it "should return False because b is not in a" $ do
      ("abc" `contains` "d") `shouldBe` False
      ("Ollie is awesome" `contains` "Rocks") `shouldBe` False
      ("Kopf" `contains` "head") `shouldBe` False
  describe "indexOf" $ do
    it "should return Just x because b is in a" $ do
      ("abc" `indexOf` "b") `shouldBe` Just 1
      ("Ollie is awesome" `indexOf` "Ollie") `shouldBe` Just 0
      ("Spongehead" `indexOf` "head") `shouldBe` Just 6
    it "should return Nothing because b is not in a" $ do
      ("abc" `indexOf` "d") `shouldBe` Nothing
      ("Ollie is awesome" `indexOf` "Rocks") `shouldBe` Nothing
      ("Kopf" `indexOf` "head") `shouldBe` Nothing