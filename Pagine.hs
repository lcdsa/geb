module Pagine where

import Data.Maybe

import MIU 
import PG
import NumberSequence

import Test.QuickCheck

pag :: Int -> IO ()
pag 36 = do
      putStrLn "Ecco alcune stringhe del sistema MIU:"
      sample (arbitrary :: Gen MIUString)
      putStrLn ""
pag 80 = do
      putStrLn $ "Ecco un rompicapo su cui riflettere (...): " 
            ++ "come si può caratterizzare il seguente insieme di numeri "
            ++ "interi (o il suo spazio negativo)?"
      putStrLn ""
      print figure
      putStrLn ""
      putStrLn "Qualche indizio/osservazione:"
      putStrLn $ "- lo \"sfondo\" (o complemento) di questa \"figura\": " 
              ++ (show $ background figure)
      putStrLn $ "- il numero successivo: " ++ (show $ fromJust $ next figure)
      putStrLn "- gli N numeri successivi (inserire N qui sotto): "
      n <- getLine
      print $ continue' figure (read n :: Int)
pag _ = putStrLn "Niente di interessante..."