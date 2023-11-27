module Data.Extra.List
  ( contains,
    indexOf,
    startsWith,
  )
where

contains :: Eq a => [a] -> [a] -> Bool
contains [] _ = False
contains _ [] = True
contains a b = a `startsWith` b || contains (tail a) b

indexOf :: Eq a => [a] -> [a] -> Maybe Int
indexOf [] _ = Nothing
indexOf _ [] = Nothing
indexOf a b
  | not $ contains a b = Nothing
  | a `startsWith` b = Just 0
  | otherwise = fmap (+ 1) (indexOf (tail a) b)

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith a b = take (length b) a == b
