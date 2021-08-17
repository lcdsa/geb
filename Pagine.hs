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
   ]
--------------------- Introduzione -------------------------------
pag 6 = do
   putStr $ unlines 
      [ ""
      , "Il Tema Regio"
      , ""
      , "> askThenPlay royalTheme"
      ]
   askThenPlay royalTheme 
pag 9 = do
   putStr $ unlines 
      [ ""
      , "Canone inverso di Scott E. Kim sul tema Good King Wenceslas"
      , "> askThenPlay (theme :=: variation)"
      ] 
   askThenPlay (theme :=: variation)
   putStr $ unlines
      [ ""
      , "Canone inverso \"algoritmico\" sul tema Good King Wenceslas"
      , "> askThenPlay (inverseCanon (2/4) theme)"
      ]
   askThenPlay (inverseCanon (2/4) theme)
---------------------- Capitolo I --------------------------------
pag 36 = do
   putStr $ unlines
      [ ""
      , "Il sistema MIU utilizza soltanto tre lettere"
         ++ " dell'alfabeto: M, I e U:"
      , "> \"MIU\" :: MIU.String"
      ]
   print ("MIU" :: MIU.String)
   putStr $ unlines
      [ "> \"abc\" :: MIU.String"
      , "*** Exception: Prelude.undefined"
      ]
{-
      , ""
      , "Una stringa del sistema MIU generata casualmente:"
      , "> generate randomString"
      ]
   x <- generate randomString
   print x
   putStrLn "> generate randomString"
   y <- generate randomString
   print y
   putStrLn "> s <- generate randomString"
   z <- generate randomString
   putStrLn "> s"
   print z
   putStrLn "> s"
   print z
-}
   putStr $ unlines
      [ ""
      , "Altre possibili stringhe del sistema MIU:"
      , "> sample randomString"
      ]
   sample randomString
   
pag 37 = putStr $ unlines
   [ ""
   , "Stringhe \"in nostro possesso\":"
   , "> MIU.axioms"
   , show $ MIU.axioms
   , ""
   , "Regole di derivazione:"
   , ""
   , "Regola I: Se si possiede una stringa che termina con una I,"
      ++ " si può aggiungere una U alla fine."
   , "> rule 1 \"MI\""
   , show $ rule 1 "MI"
   , ""
   , "Regola II: Si abbia Mx."
      ++ " Allora si può includere Mxx nella collezione."
   , "> rule 2 \"MIU\""
   , show $ rule 2 "MIU"
   , ""
   , "Regola III: Se in una delle stringhe"
      ++ " della collezione c'è III,"
      ++ " si può costruire una nuova stringa"
      ++ " sostituendo U al posto di III." 
   , "> rule 3 \"UMIIIMU\""
   , show $ rule 3 "UMIIIMU"
   ]
pag 38 = putStr $ unlines
   [ ""
   , "Regola IV: Se all'interno di una delle"
      ++ " stringhe della collezione c'è UU,"
      ++ " si può eliminarlo."
   , "> rule 4 \"UUU\""
   , show $ rule 4 "UUU"
   , ""
   , "Chiamiamo le stringhe in nostro possesso \"assiomi\"."
   , "> MIU.axioms"
   , show MIU.axioms
   , ""
   , "Gli assiomi sono teoremi."
   , "> all (isKnownTrue . isTheorem) MIU.axioms"
   , show $ all (isKnownTrue . isTheorem) MIU.axioms
   ]
pag 39 = putStr $ unlines
   [ ""
   , "La string MUIIU è un teorema:"
   , "> isTheorem' \"MUIIU\" (derivation [2,2,1,3,2,4])"
   , show $ isTheorem' "MUIIU" (derivation [2,2,1,3,2,4])
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
   , "come si può caratterizzare il seguente"
      ++ " insieme di numeri interi?"
      ++ " (o il suo spazio negativo)"
   , ""
   , show figure
   , ""
   , "Qualche indizio/osservazione:"
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
      ++ " di Lewis Carrol, a cura di Arianna Masciolini"
   , ""
   , "-----------------------------------------"
   , ""
   , jabberwocky
   ]
---------------------------- FINE ---------------------------------
pag n = putStr $
   if n `elem` index 
   then "\nCi stiamo lavorando...\n"
   else unlines
      [ ""
      , "Questa pagina non ha contenuti implementabili."
      , ""
      , showIndex
      ]

main :: IO ()
main = browse 0

browse :: Int -> IO ()
browse n = do 
   putStrLn "-----------------------------------------"
   putStrLn $ " Pagina " ++ show  n ++ ":"
   putStrLn "-----------------------------------------"
   pag n 
   putStrLn ""
   browse' n
 where
   browse' :: Int -> IO ()
   browse' n = do
      putStrLn "-----------------------------------------"
      putStr "(n) avanti, (p) indietro, (q) esci: "
      c <- getChar
      putStrLn ""
      case c of
         'n' -> browse (n+1)
         'p' -> if n > 0 then browse (n-1) else browse n
         'q' -> return ()
         _   -> browse' n 


askThenPlay :: Music Pitch -> IO ()
askThenPlay mp = do
   putStr "Eseguire traccia musicale? (s/n): "
   c <- getChar
   putStrLn "\n"
   case c of
      's' -> play mp
      ___ -> return ()