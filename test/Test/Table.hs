module Test.Table(TableTest (..), describeTable) where

import Test.Hspec
import Data.Foldable (traverse_)

data TableTest a b = TableTest
  { name :: String,
    given :: a,
    want :: b
  }

describeTable :: Show b => Eq b => String -> (a -> IO b) -> [TableTest a b] -> SpecWith ()
describeTable n tf tts = describe n $ do
  traverse_ (\tt -> it (name tt) (runTest tf tt)) tts

runTest :: Show b => Eq b => (a -> IO b) -> TableTest a b -> IO ()
runTest tf tt = do
  out <- tf $ given tt
  out `shouldBe` want tt
