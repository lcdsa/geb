module Pagine where

import MIU 
import PG

import Test.QuickCheck

pag :: Int -> IO ()
pag 36 = do
      putStrLn "Ecco alcune stringhe del sistema MIU:"
      sample (arbitrary :: Gen MIUString)
      putStrLn ""
pag _ = putStrLn "Niente di interessante..."