module Pagine where

import Data.Maybe
import Euterpea hiding (play)

import Eutermea
import MIU 
import PG
import NumberSequence
import Bach (royalTheme)
import GoodKingWenceslas (theme, variation)
import Jabberwocky (jabberwocky)

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
   putStrLn "Alcune stringhe del sistema MIU:"
   sample (arbitrary :: Gen MIU.String)
   putStrLn ""
pag 37 = putStr $ unlines
   [ ""
   , "Stringhe in nostro possesso:"
   , ""
   , unlines $ map show axioms

   , "Regole di derivazione:"
   , ""
   , "Regola I: Se si possiede una stringa che termina con una I,"
      ++ " si può aggiungere una U alla fine."
   , ""
   , "Regola II: Si abbia Mx."
      ++ " Allora si può includere Mxx nella collezione."
   , ""
   , "Esempi:"
   , unlines $ map 
      ( \s -> "Da " ++ s ++ " si può ottenere "
               ++ (show . fromJust) (rule 2 (read s))
               ++ "."
      ) 
      ["MIU", "MUM", "MU"]
   , "Regola III: Se in una delle stringhe della collezione c'è III,"
      ++ " si può costruire una nuova stringa sostituendo U al posto di III."
   , ""
   , "Esempi:"
   , unlines $ map
      ( \s -> let s' = rule 3 (read s) in
         "A partire da " ++ s 
         ++ (if isNothing s' then " non"   else "" ) ++ " si può costruire "
         ++ (if isNothing s' then "niente" else show $ fromJust s')
         ++ "."
      ) 
      ["UMIIIMU","MIIII","IIMII","MIII"]
   ]
pag 38 = putStr $ unlines
   [ ""
   , "Regola IV: Se all'interno di una delle stringhe della collezione c'è UU,"
      ++ " si può eliminarlo."
   , ""
   , "Esempi:"
   , unlines $ map 
      ( \s -> "Da " ++ s ++ " si ottiene "
               ++ (show . fromJust) (rule 4 (read s))
               ++ "."
      ) 
      ["UUU", "MUUUIII"]   
   ]
pag 39 = putStr $ unlines
   [ ""
   , "Ecco una derivazione del teorema MUIIU:"
   , show (sequential [2,2,1,3,2,4])
   ]
pag 80 = putStr $ unlines
   [ ""
   , "Ecco un rompicapo su cui riflettere (...): " 
   , "come si può caratterizzare il seguente insieme di numeri "
      ++ "interi (o il suo spazio negativo)?"
   , ""
   , show figure
   , ""
   , "Qualche indizio/osservazione:"
   , ""
   , "- lo \"sfondo\" (o complemento) di questa \"figura\": " 
   , show (background figure)
   , "- il numero successivo: " ++ show (fromJust $ next figure)
   , "- la sequenza \"allungata\" di 5: "
   , show (continue' figure 5)
   ]
pag 397 = putStr $ unlines
   [ ""
   , "IL MASCELLONTE"
   , "traduzione italiana alternativa del \"Jabberwocky\""
      ++ "di Lewis Carrol, a cura di Arianna Masciolini"
   , ""
   , "-----------------------------------------"
   , ""
   , jabberwocky
   ]
pag _ = putStrLn "Niente di interessante..."

main = undefined
