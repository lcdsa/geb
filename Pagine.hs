module Pagine where

import MIU 
import PG

import Test.QuickCheck

pag :: Int -> IO ()
pag 36 = do
   putStrLn "Alcune stringhe del sistema MIU:"
   sample (arbitrary :: Gen MIU.String)
   putStrLn ""
pag 37 = do
   putStrLn "Stringhe in nostro possesso:"
   putStrLn ""
   putStrLn $ unlines $ map show theorems

pag _ = putStrLn "Niente di interessante..."