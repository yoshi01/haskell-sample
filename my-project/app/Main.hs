module Main where

import Lib
import Prelude hiding (map)

map :: (a -> b) -> [a] -> [b]
map _[] = []
map f (x:xs) = f x : map f xs

main :: IO ()
main = do
  print $ map id [1,2,3,4,5]
  print $ map succ [1,2,3,4,5]
  print $ map (\x -> x * x) [1,2,3,4,5]
