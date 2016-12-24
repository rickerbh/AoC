module Main where

import qualified AoC201601 as AoC01
import qualified AoC201602 as AoC02
import qualified AoC201603 as AoC03
import qualified AoC201604 as AoC04
import qualified AoC201605 as AoC05

main :: IO ()
main = do
  AoC01.runDay
  AoC02.runDay
  AoC03.runDay
  AoC04.runDay
  putStrLn "Next step takes a while..."
  AoC05.runDay
