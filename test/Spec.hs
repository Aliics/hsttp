{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

data TableTest a b = TableTest
  { name :: String,
    given :: a,
    want :: b
  }

runTest :: Show b => Eq b => (a -> IO b) -> TableTest a b -> IO ()
runTest tf tt = do
  out <- tf $ given tt
  out `shouldBe` want tt

describeTable :: Show b => Eq b => String -> (a -> IO b) -> [TableTest a b] -> SpecWith ()
describeTable n tf tts = describe n $ do
  traverse_ (\tt -> it (name tt) (runTest tf tt)) tts
