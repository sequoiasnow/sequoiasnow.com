module Main where

import Api.Server       ( runApplication )
import Api.Config       ( getConfig )     

main :: IO ()
main = do
  c <- getConfig
  runApplication c
  
