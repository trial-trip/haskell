module Main where

import Stepik_1

main :: IO ()
main = do
  print $ take 10 [Odd (10^20 + 7), Odd (10^20 + 11) .. Odd (10^20 + 17)]
  print $ take 10 [Odd (10^20 + 17), Odd (10^20 + 13) .. Odd (10^20 + 7)]
  print $ take 5 [Odd (10^20 + 7) .. Odd (10^20 + 17)]
