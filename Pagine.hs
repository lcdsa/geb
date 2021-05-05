module Pagine where

import Data.List
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

index = 
 [ 6, 9                 -- Introduzione: un'offerta musico-logica
 , 36, 37, 38, 39, 43   -- Capitolo I - Il gioco MU
 , 50, 51, 57           -- Capitolo II - Significato e forma in matematica
 , 80                   -- Capitolo III - Figura e sfondo
 , 397                  -- Suite anglo-franco-italo-tedesca
 ]

showIndex = "Pagine con contenuti implementabili:\n"
   ++ intercalate ", " (map show index)

toc :: IO () 
toc = pag 0

pag :: Int -> IO ()
pag 0 = putStr $ unlines
   [ "-----------------------------------------"
   , "Contenuti implementabili per il libro"
   , ""
   , "\"Gödel, Escher, Bach: "
   , "\tun'Eterna Ghirlanda Brillante\""
   , ""
   , " di Douglas R. Hofstadter."
   , "-----------------------------------------"
   , ""
   , showIndex
   , ""
   ]
--------------------- Introduzione -------------------------------
pag 6 = do
   putStrLn "\nIl Tema Regio"
   play royalTheme 
pag 9 = do
   putStrLn "\nCanone inverso di Scott E. Kim sul tema Good King Wenceslas"
   play (theme :=: variation)
   putStrLn "\nCanone inverso \"algoritmico\" sul tema Good King Wenceslas"
   play (inverseCanon (2/4) theme)
---------------------- Capitolo I --------------------------------
pag 36 = do
   putStr $ unlines
      [ ""
      , "La prima cosa da dire del nostro sistema formale, il \"sistema MIU\""
         ++ " è che esso utilizza soltanto tre lettere dell'alfabeto:"
      , intercalate ", " (map show [M,I,U])
      , ""
      , "Ecco alcune stringhe del sistema MIU:"
      ]
   sample (arbitrary :: Gen MIU.String)
   putStrLn ""
pag 37 = putStr $ unlines
   [ ""
   , "Ma sebbene tutte queste stringhe siano legittime,"
      ++ " non sono stringhe \"in nostro possesso\"."
   , "In effetti, l'unica stringa fin'ora in nostro possesso è:"
   , (show . head) MIU.axioms
   , ""
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
      ( \s -> let s' = rule 3 (read s) in concat
         [ "A partire da ", s 
         , if isNothing s' then " non" else "", " si può costruire "
         , maybe "niente" show s', "."
         ]
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
      ( \s -> concat
         [ "Da ",  s , " si ottiene "
         , (show . fromJust) (rule 4 (read s)) , "."
         ]
      ) 
      ["UUU", "MUUUIII"]   
   ]
pag 39 = putStr $ unlines
   [ ""
   , "Ecco una derivazione del teorema MUIIU:"
   , show (sequential [2,2,1,3,2,4])
   ]
----------------------- Capitolo II --------------------------------
pag 50 = do
   putStrLn "\nAlcune stringhe del sistema pg:"
   sample (arbitrary :: Gen PG.String)
   putStrLn ""
----------------------- Capitolo III -------------------------------
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
-------------------------- Jabberwocky ----------------------------
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
---------------------------- FINE ---------------------------------
pag n = putStrLn $
   if n `elem` index 
   then "\nCi stiamo lavorando...\n"
   else unlines
      [ ""
      , "Questa pagina non ha contenuti implementabili."
      , ""
      , showIndex
      ]

main :: IO ()
main = toc
