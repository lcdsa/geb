module Pagine where

import Data.Maybe
import Euterpea hiding (play)

import Eutermea
import MIU 
import PG
import NumberSequence
import Bach (royalTheme)
import GoodKingWenceslas (theme, variation)

import Test.QuickCheck

pag :: Int -> IO ()
pag 6 = do
      putStrLn "Il Tema Regio"
      play royalTheme 
pag 9 = do
      putStrLn "Canone inverso di Scott E. Kim sul tema Good King Wenceslas"
      play $ theme :=: variation
      putStrLn "Canone inverso \"algoritmico\" sul tema Good King Wenceslas"
      play $ inverseCanon (2/4) theme
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
      print $ background figure
      putStrLn $ "- il numero successivo: " ++ show (fromJust $ next figure)
      putStrLn "- la sequenza \"allungata\" di 5: "
      print $ continue' figure 5
pag _ = putStrLn "Niente di interessante..."

main = undefined